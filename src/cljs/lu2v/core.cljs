(ns lu2v.core
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [clojure.string :as string]))

(enable-console-print!)

(defn norm-str [s]
  (.. s
    (toLowerCase)
    (normalize "NFD")
    (replace (js/RegExp "[\u0300-\u036f]" "g") "")
    (replace "ς" "σ")))

(comment
  (norm-str "Κώστας"))

(def greek-alphabet   "αβγδεζηθικλμνξοπρστυφχψω")
(def english-alphabet "abcdefghijklmnopqrstuvwxyz")

(comment
  (string/join
    (for [i (range 97 (+ 97 26))]
      (String/fromCharCode i))))

;; app

(def click-count (r/atom 0))

(def en-lang "en-US")
(def el-lang "el-GR")

(def supported-langs #{en-lang el-lang})

(def browser-lang (.-language js/navigator))

(println "browser lang" browser-lang)

(defn calc-lang [locale]
  (condp = locale
    en-lang english-alphabet
    el-lang greek-alphabet
    english-alphabet))

(def default-lang (calc-lang browser-lang))

(defonce state (r/atom {:name1 ""
                        :name2 ""
                        :alphabet default-lang
                        :lang (or (supported-langs browser-lang) en-lang)}))

(def i18n
  {:name-1 {en-lang "Name 1"
            el-lang "Όνομα 1"}
   :name-2 {en-lang "Name 2"
            el-lang "Όνομα 2"}
   :what-is-this {en-lang "What is this?"
                  el-lang "Τι είναι αυτό;"}
   :footer-text {en-lang [:div
                          [:p.mb-4 "Build with ❤️ using "
                           [:a {:href "#"} "cljs"]
                           ", "
                           [:a {:href "#"} "bulma"]
                           " and "
                           [:a {:href "#"} "reagent"]]
                          [:p.mb-4 "This app respects your privacy"]
                          [:p
                           [:a {:href "https://github.com/kongeor/lu2v"} "Source code"]]]
                 el-lang [:div
                          [:p.mb-4 "Φτιαγμένο με ❤️ χρησιμοποιώντας "
                           [:a {:href "#"} "cljs"]
                           ", "
                           [:a {:href "#"} "bulma"]
                           " και "
                           [:a {:href "#"} "reagent"]]
                          [:p.mb-4 "Αυτή η εφαρμογή σέβεται την ιδιωτικότητά σας"]
                          [:p
                           [:a {:href "https://github.com/kongeor/lu2v"} "Πηγαίος κώδικας"]]]}})

(def famous-couples
  {en-lang [["Adam" "Eve"]
            ["Foo" "Bar"]]
   el-lang [["Αδάμ" "Έυα"]
            ["Φου" "Μπαρ"]]})

(defn i [k]
  (get-in i18n [k (:lang @state)] "?"))

(defn name-input-component []
  (let [couples (get famous-couples (:lang @state))
        [p1 p2] (shuffle (first (shuffle couples)))]
    [:div.mb-4
     [:div.field
      [:label.label (i :name-1)]
      [:div.control
       [:input.input {:type        "text"
                      :value       (:name1 @state)
                      :placeholder p1
                      :on-change   #(swap! state assoc :name1 (-> % .-target .-value))
                      }]]]
     [:div.field
      [:label.label (i :name-2)]
      [:div.control
       [:input.input {:type        "text"
                      :value       (:name2 @state)
                      :placeholder p2
                      :on-change   #(swap! state assoc :name2 (-> % .-target .-value))
                      }]]]]))

(defn str->int [s]
  (js/parseInt s))

(defn reduce-str [s]
  (loop [r []
         i 0
         j (dec (count s))]
    (cond
      (< j i) (string/join r)
      (= j i) (recur (conj r (get s i)) (inc i) (dec j))
      :else
      (let [res (+ (str->int (get s i)) (str->int (get s j)))]
        (recur (conj r (str res)) (inc i) (dec j))))))

(comment
  (reduce-str "")
  (reduce-str "1234")
  (reduce-str "108")
  (reduce-str (reduce-str "12345")))

(comment
  (let [s "123"]
    (when (or (= s "100") (< (count s) 3))
      s))
  )

(defn calc-perc [fqs]
  (let [s (string/join (map #(get fqs % 0) (:alphabet @state)))]
    (loop [r [s]
           s s]
      (cond
        (or (= s "100") (< (count s) 3))
        {:perc (str->int s)
         :iters r}

        :else
        (let [s' (reduce-str s)]
          (recur (conj r s') s'))))))

(comment
  (calc-perc {"α" 5})
  (calc-perc {"α" 2 "ω" 9}))

(defn calc-perc-class [perc]
  (condp < perc
    90 :is-success
    65 :is-info
    35 :is-warning
    :is-danger))

(comment
  (calc-perc-class "0"))

(defn letter-freqs []
  (let [{:keys [name1 name2 alphabet]} @state
        fqs (frequencies (norm-str (str name1 name2)))]
    [:div.mb-4
     [:table.table.is-fullwidth
      [:tbody
       (for [x (range (Math/ceil (/ (count alphabet) 6)))]
         ^{:key (str "tr-" x)} [:tr
          (for [y (range 6)]
            (let [idx (+ (* 6 x) y)
                  letter (get alphabet idx)]
              (when letter
                ^{:key (str "td-" idx)} [:td (str letter " - " (get fqs letter 0))])))])]]
     (let [{:keys [perc iters]} (calc-perc fqs)]
       [:div
        [:div.tag.is-large.mb-4 {:class (calc-perc-class perc)} (str perc "%")]
        [:div
         (map-indexed (fn [idx item]
                        ^{:key idx} [:p item]) iters)]])]))

(defn toggle-lang [locale]
  (swap! state assoc :lang locale :alphabet (calc-lang locale)))

(defn navbar [locale]
  [:nav.navbar.mb-4 {:role "navigation" :aria-label "main navigation"}
   [:div.navbar-brand
    [:a.navbar-item {:href "https://lu2v.cons.gr"}
     [:h1.is-size-4 "lu2v"]
     #_[:img {:src "https://bulma.io/images/bulma-logo.png" :width "112" :height "28"}]]
    [:a.navbar-burger {:role "button" :aria-label "menu" :aria-expanded "false" :data-target "navbarBasicExample"}
     [:span {:aria-hidden "true"}]
     [:span {:aria-hidden "true"}]
     [:span {:aria-hidden "true"}]]]
   [:div#navbarBasicExample.navbar-menu
    [:div.navbar-start
     [:a.navbar-item {:on-click #(toggle-lang en-lang) :class (when (= en-lang locale)
                                                                "is-underlined")} "English"]
     [:a.navbar-item {:on-click #(toggle-lang el-lang) :class (when (= el-lang locale)
                                                                "is-underlined")} "Ελληνικά"]]
    [:div.navbar-end
     [:a.navbar-item {:href "https://blog.cons.gr/posts/2022-03-20-lu2v/"} (i :what-is-this)]]]])

(defn scripts []
  (when-not js/goog.DEBUG
    [:<>
     [:noscript
      [:img {:src "https://stats.cons.gr/ingress/611fb17e-0cfb-44bb-9a23-089dc4a9c9d5/pixel.gif"}]]
     [:script {:defer true :src "https://stats.cons.gr/ingress/611fb17e-0cfb-44bb-9a23-089dc4a9c9d5/script.js"}]]))

(defn app-comp []
  (let [{:keys [lang]} @state]
    [:<>
     [:section.section
      [:div.container
       [:div.columns
        [:div.column.is-half.is-offset-one-quarter
         [navbar lang]
         [name-input-component]
         [letter-freqs]
         [:footer.footer
          (i :footer-text)]]]]]
     [scripts]]))

(comment
  (type (first (seq "α"))))

(comment
  (.normalize "ά" "NFD"))

(comment
  ;; why?
  #_(.. "ϋάκοΑΣΔΦΧΨΩύά" (toLowerCase) (normalize "NFD") (replace (js/RegExp "[\u0300-\u036f]" "g") ""))
  (frequencies (norm-str "ϋάκοΑΣΔΦΧΨΩύά")))

(comment
  #"[\u0300-\u036f]")

(comment
  (+ 1 1))

(comment
  (reset! click-count -1))

(comment
  (js/alert "yo"))

(defn ^:export init []
  (rd/render [app-comp] (js/document.getElementById "app")))