(ns defcomponent
  (:require [com.stuartsierra.component :as component]))

;; Обертка вокруг com.stuartsierra.component, где зависимости между компонентами
;; определяются в коде определения компонента, а система строится динамически,
;; исходя из запрашиваемых компонентов.
;; Для больших систем таким образом убивается большое количество мусорного кода,
;; одновременно уменьшая количество багов (мы не можем забыть объявить
;; зависимость или опечататься в ее названии). Добавляются
;; некоторые соглашения: так каждому конструктору компонента может скармливаться
;; объект, загруженный из конфиг-файла
;;
;; (defcomponent db []
;;   [config]
;;   (start [this] {:connection :im-connection!})
;;   (stop [this]))
;;
;; (defcomponent app [db]
;;   [config]
;;   (start [this])
;;   (stop [this]))
;;
;; defcomponent app:
;; - определяет record app-component
;; - определяет для этого рекорда протокол Defcomponent, возвращающий
;; мета-информацию о компоненте
;; - функцию-конструктор app, принимающую config как параметр и возвращающую
;; компонент. параметры конструктора будут доступны в рекорде
;;
;; Символ в списке зависимостей является сокращением для [:dependant db :db].
;; Значит компонент с конструктором db будет определен как зависимость текущего
;; компонента и доступен по ключу :db.
;;
;; Так же возможно следующие описание [:inject-to big-app :small-app]. Значит
;; компоненту big-app будет добавлена зависимость от текущего компонента в виде
;; ключа :small-app
;;
;; Альтернативой :inject-to является :pass-to, который позволяет компонету
;; передать свою зависимость другим. Например, для примера выше, если какому-то
;; компоненту big-app, например tracer, необходимы какие-то данные от small-app,
;; в объявлении big-app можно объявить [:pass-to tracer :small-app :as :target].
;; Таким образом можно строить подсистемы, которые протаскивают переданные зависимости
;; своим подкомпонентам, но не заставляют пользователя знать о внутреннем устройстве
;; этой подсистемы.
;; :pass-to можно также использовать для передачи зависимости всем детям:
;; [:pass-to :all :small-app :as :target] -- так компонент, переданный в big-app
;; как :small-app будет передан всем завивимостям big-app (кроме самой small-app).
;;
;; Формирование системы происходит автоматически исходя из списка необходимых компонентов.
;;
;; (system [app])
;; Сформирует систему состояющую из app и db, с проставленными зависимостями и
;; запустит ее.
;;
;; system вторым аргументом принимает параметры:
;; :start сразу же стартует систему
;; :file-config сделает load-file на указаный путь и результат будет
;; передаваться в качестве аргумента в конструкторы компонентов
;; :params параметры, передаваемые в конструктор компонента
;; :repo набор компонентов, которые будут замержены с теми, что мы получили
;; проходя по графу зависимостей перед формированием системы. можно
;; использовать, например, в тестах для замены компонентов на мокнутые
;;
;; Формат :params: ключ это конструктор компонента, на который ссылаются другие
;; компоненты, значение это либо новый конструктор, либо [новый-конструктор
;; зависимости]
;; (system [app] {:start true
;;                :file-config "config/hello.clj"
;;                :params [1 2 3]
;;                :repo {db mocked-db}})
;;
;; TODO: свои конструкторы
;; TODO: динамическое формирование с ::specs
;; TODO: примеры

(defprotocol Defcomponent
  (component-keyword [this])
  (component-specs [this]))

(extend-type Object
  Defcomponent
  (component-keyword [this] (throw (Exception. "Can't infer name for unknown components")))
  (component-specs [_] []))

(defn normalize-specs
  [specs]
  (let [normalize-spec (fn [spec] (if (vector? spec)
                                   spec
                                   [:dependant spec]))]
    (map normalize-spec specs)))

(defmacro defcomponent
  [n specs & [constructor-binding & body]]
  (let [[lifecycle-description other-protocols] (split-with (comp not symbol?) body)
        constructor-binding' (or constructor-binding '[& _])
        record-sym (symbol (str n "-record"))
        record-constructor (symbol (str "->" record-sym))]
    `(do
       (defrecord ~record-sym ~(or constructor-binding [])
         Defcomponent
         (component-keyword [_] ~(keyword n))
         (component-specs [this#]
           (concat ~specs (::specs this#)))
         ~@(when (seq lifecycle-description)
             (cons `component/Lifecycle lifecycle-description))
         ~@other-protocols)
       (defn ~n ~constructor-binding' (~record-constructor ~@(or constructor-binding []))))))

(defn- map-vals
  [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn- dependant-by-alias [component alias]
  (let [specs (::specs component)
        pred (fn [spec] (if-let [[f found last] spec]
                          (when (and (= f :dependant) (= last alias))
                            found)))]
    (if-let [found (first (keep pred specs))]
      found
      (throw (Exception. (str "Can't pass unknown component " alias))))))

(defn- all-dependants-except-alias [component key]
  (let [specs (::specs component)
        pred (fn [[f dep last]]
               (when (and (= f :dependant) (not= last key))
                 dep))]
    (keep pred specs)))

(defn- apply-pass-to [r constructor component spec]
  (let [[_ to what _ as] spec
        found (dependant-by-alias component what)]
    (if (= :all to)
      (let [tos (all-dependants-except-alias component what)]
        (reduce
         #(update-in %1 [%2 ::specs] conj [:dependant found as])
         r tos))
      (update-in r [to ::specs] conj [:dependant found as]))))

;; чатично сортирует ключи в репозитории, таким образом, чтобы
;; компоненты с :pass-to были ДО компонентов куда им надо передать
(defn- keys-sorted-by-pass-to [repository]
  (let [get-target-for ;; цели для прокидывания
        (fn [comp spec]
          (let [[k to what _ as] spec]
            (when (= k :pass-to)
              (if (= to :all)
                (all-dependants-except-alias comp what)
                to))))
        pass-to-deps ;; мапинг ключа репозитория на pass-to зависимости
        (into {} (for [[cons comp] repository
                       :let [pass-specs (flatten
                                         (keep (partial get-target-for comp)
                                               (::specs comp)))]]
                   (when (not-empty pass-specs)
                     [cons (set pass-specs)])))
        cmp (fn [a b]
              (let [adeps (get pass-to-deps a)
                    bdeps (get pass-to-deps b)]
                (cond
                  ;; если b есть среди зависимостей a -- a должен быть первым
                  (and adeps (adeps b))
                  -1
                  ;; и наоборот
                  (and bdeps (bdeps a))
                  1
                  :else ;; иначе не трогаем
                  0)))]
    (sort cmp (keys repository))))

;; возращает новый репозиторий, в котором все :inject-to спеки и
;; :pass-to спеки превращены в dependant спеки соответсвующих компонентов.
(defn- apply-rules
  [repository]

  (reduce
   (fn [r constructor]
     (let [component (get r constructor)]
       (reduce (fn [r' spec]
                 (cond
                   (= :inject-to (first spec))
                     (let [[_ to as] spec]
                       (update-in r' [to ::specs] conj [:dependant constructor as]))

                   (= :pass-to (first spec))
                   (apply-pass-to r' constructor component spec)

                   :else r'))
               r (normalize-specs (component-specs component)))))
   repository (keys-sorted-by-pass-to repository)))


(defn- constructor-params
  [file-config params]
  (cond->> params
    file-config (cons (load-file file-config))))

;; Репозитарий это мапа, где ключ это конструктор, а значение - компоненты,
;; полученные путем вызова конструктора
(defn- make-repository
  [constructors constructor-params additions]
  (loop [[constructor & constructors'] constructors acc {}]
    (cond
      (nil? constructor) acc
      (get acc constructor) (recur constructors' acc)
      :else
      (let [component (or (get additions constructor)
                          (apply constructor constructor-params))
            specs (normalize-specs (component-specs component))
            component' (assoc component ::specs specs)]
        (recur (into constructors' (->> specs
                                        (filter #(= :dependant (first %)))
                                        (map second)))
               (assoc acc constructor component'))))))

(defn- component-dependencies-map
  [repository component]
  (let [specs (->> (::specs component)
                   (filter #(= :dependant (first %))))]
    (into {} (for [[_ dependant-constructor key] specs
                   :let [key' (or key (component-keyword (get repository dependant-constructor)))]]
               [key' dependant-constructor]))))

(defn- ->system
  [repository]
  (let [system (into (component/system-map) repository)]
    (into system
          (for [[constructor component] system]
            [constructor
             (component/using component (component-dependencies-map repository component))]))))

(defn system
  ([constructors] (system constructors {}))
  ([constructors {:keys [start file-config params repo]}]
   (let [params' (constructor-params file-config params)
         repository (-> (make-repository constructors params' repo)
                        apply-rules)
         system (->system repository)]
     (if start
       (component/start system)
       system))))

;; (defcomponent db []
;;   [config]
;;   (start [this] {:connection :im-connection!})
;;   (stop [this] this))

;; (defcomponent app [db]
;;   [config]
;;   (start [this] this)
;;   (stop [this] this))

;; (defcomponent middleware [[:inject-to app :middle]])

;; (def s (system [app middleware] {:start true :params {:hello 1}}))
