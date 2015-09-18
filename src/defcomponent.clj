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
;; Так же возможно следующие описание [:inject-to big-app :small-app]. Значит
;; компоненту big-app будет добавлена зависимость от текущего компонента в виде
;; ключа :small-app
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
  (component-keyword [_] (throw "Can't infer name for unknown components"))
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

;; возращает новый репозиторий, в котором все :inject-to спеки превращены в
;; dependant спеки соответсвующий компонентов.
(defn- apply-injections
  [repository constructors]
  (reduce (fn [r constructor]
            (let [component (get repository constructor)]
              (reduce (fn [r' spec]
                        (if (= :inject-to (first spec))
                          (let [[_ to as] spec]
                            (update-in r' [to ::specs] conj [:dependant constructor as]))
                          r'))
                      r (normalize-specs (component-specs component)))))
          repository constructors))

(defn- constructor-params
  [file-config params]
  (cond->> params
    file-config (cons (load-file file-config))))

;; Репозитарий это мапа, где ключ это конструктор, а значение - компоненты,
;; полученные путем вызова конструктора
(defn- make-repository
  [constructors constructor-params]
  (loop [[constructor & constructors'] constructors acc {}]
    (cond
      (nil? constructor) acc
      (get acc constructor) (recur constructors' acc)
      :else
      (let [component (apply constructor constructor-params)
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
         repository (-> (make-repository constructors params')
                        (merge (map-vals normalize-specs repo))
                        (apply-injections constructors))
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


