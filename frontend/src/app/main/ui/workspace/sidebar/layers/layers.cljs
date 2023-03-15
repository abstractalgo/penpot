;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.sidebar.layers.layers
  (:require-macros [app.main.style :refer [css styles]])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.uuid :as uuid]
   [app.main.data.workspace :as dw]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.shape-icon-refactor :as sic]
   [app.main.ui.hooks :as hooks]
   [app.main.ui.icons :as i]
   [app.main.ui.workspace.sidebar.layers.layer-item.layer-item :refer [layer-item]]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [app.util.timers :as ts]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

;; --- Layer Name

(def shape-for-rename-ref
  (l/derived (l/in [:workspace-local :shape-for-rename]) st/state))

(mf/defc layer-name
  [{:keys [shape on-start-edit  disabled-double-click on-stop-edit name-ref depth parent-size] :as props}]
  (let [local            (mf/use-state {})
        shape-for-rename (mf/deref shape-for-rename-ref)

        start-edit (fn []
                     (when (not disabled-double-click)
                       (on-start-edit)
                       (swap! local assoc :edition true)))

        accept-edit (fn []
                      (let [name-input (mf/ref-val name-ref)
                            name       (dom/get-value name-input)]
                        (on-stop-edit)
                        (swap! local assoc :edition false)
                        (st/emit! (dw/end-rename-shape)
                                  (when-not (str/empty? (str/trim name))
                                    (dw/update-shape (:id shape) {:name (str/trim name)})))))
        cancel-edit (fn []
                      (on-stop-edit)
                      (swap! local assoc :edition false)
                      (st/emit! (dw/end-rename-shape)))

        on-key-down (fn [event]
                      (when (kbd/enter? event) (accept-edit))
                      (when (kbd/esc? event) (cancel-edit)))
        space-for-icons 110
        parent-size (str (- parent-size space-for-icons) "px")]

    (mf/with-effect [shape-for-rename]
      (when (and (= shape-for-rename (:id shape))
                 (not (:edition @local)))
        (start-edit)))

    (mf/with-effect [(:edition @local)]
      (when (:edition @local)
        (let [name-input (mf/ref-val name-ref)]
          (dom/select-text! name-input)
          nil)))

    (if (:edition @local)
      [:input
       {:class (dom/classnames (css :element-name-input) true)
        :style #js {"--depth" depth "--parent-size" parent-size}
        :type "text"
        :ref name-ref
        :on-blur accept-edit
        :on-key-down on-key-down
        :auto-focus true
        :default-value (:name shape "")}]
      [:span
       {:class (dom/classnames (css :element-name) true)
        :style #js {"--depth" depth "--parent-size" parent-size}
        :ref name-ref
        :on-double-click start-edit}
       (:name shape "")
       (when (seq (:touched shape)) " *")])))
;; This components is a piece for sharding equality check between top
;; level frames and try to avoid rerender frames that are does not
;; affected by the selected set.
(mf/defc frame-wrapper
  {::mf/wrap-props false
   ::mf/wrap [mf/memo
              #(mf/deferred % ts/idle-then-raf)]}
  [props]
  [:> layer-item props])

(mf/defc layers-tree
  {::mf/wrap [#(mf/memo % =)
              #(mf/throttle % 200)]}
  [{:keys [objects filtered? parent-size] :as props}]
  (let [selected (mf/deref refs/selected-shapes)
        selected (hooks/use-equal-memo selected)
        root (get objects uuid/zero)]
    [:ul
     {:class (dom/classnames (css :element-list) true)}
     [:& hooks/sortable-container {}
      (for [[index id] (reverse (d/enumerate (:shapes root)))]
        (when-let [obj (get objects id)]
          (if (= (:type obj) :frame)
            [:& frame-wrapper
             {:item obj
              :selected selected
              :index index
              :objects objects
              :key id
              :sortable? true
              :filtered? filtered?
              :parent-size parent-size
              :recieved-depth -1}]
            [:& layer-item
             {:item obj
              :selected selected
              :index index
              :objects objects
              :key id
              :sortable? true
              :filtered? filtered?
              :recieved-depth -1
              :parent-size parent-size}])))]]))


(mf/defc filters-tree
  {::mf/wrap [#(mf/memo % =)
              #(mf/throttle % 200)]}
  [{:keys [objects parent-size] :as props}]
  (let [selected (mf/deref refs/selected-shapes)
        selected (hooks/use-equal-memo selected)
        root (get objects uuid/zero)]
    [:ul {:class (dom/classnames (css :element-list) true)}
     (for [[index id] (d/enumerate (:shapes root))]
       (when-let [obj (get objects id)]
         [:& layer-item
          {:item obj
           :selected selected
           :index index
           :objects objects
           :key id
           :sortable? false
           :filtered? true
           :recieved-depth -1
           :parent-size parent-size}]))]))

(defn calc-reparented-objects
  [objects]

  (let [reparented-objects
        (d/mapm (fn [_ val]
                  (assoc val :parent-id uuid/zero :shapes nil))
                objects)

        reparented-shapes
        (->> reparented-objects
             keys
             (filter #(not= uuid/zero %))
             vec)]
    (update reparented-objects uuid/zero assoc :shapes reparented-shapes)))

;; --- Layers Toolbox

(defn use-search
  [page objects]
  (let [filter-state (mf/use-state {:show-search-box false
                                    :show-filters-menu false
                                    :search-text ""
                                    :active-filters #{}
                                    :num-items 100})

        clear-search-text
        (mf/use-callback
         (fn []
           (swap! filter-state assoc :search-text "" :num-items 100)))

        update-search-text
        (mf/use-callback
         (fn [event]
           (let [value (-> event dom/get-target dom/get-value)]
             (swap! filter-state assoc :search-text value :num-items 100))))

        toggle-search
        (mf/use-callback
         (fn []
           (swap! filter-state assoc :search-text "")
           (swap! filter-state assoc :active-filters #{})
           (swap! filter-state assoc :show-filters-menu false)
           (swap! filter-state assoc :num-items 100)
           (swap! filter-state update :show-search-box not)))

        toggle-filters
        (mf/use-callback
         (fn []
           (swap! filter-state update :show-filters-menu not)))

        remove-filter
        (mf/use-callback
         (mf/deps @filter-state)
         (fn [key]
           (fn [_]
             (swap! filter-state update :active-filters disj key)
             (swap! filter-state assoc :num-items 100))))

        add-filter
        (mf/use-callback
         (mf/deps @filter-state (:show-filters-menu @filter-state))
         (fn [key]
           (fn [_]
             (swap! filter-state update :active-filters conj key)
             (swap! filter-state assoc :num-items 100)
             (toggle-filters))))

        active?
        (and
         (:show-search-box @filter-state)
         (or (d/not-empty? (:search-text @filter-state))
             (d/not-empty? (:active-filters @filter-state))))

        search-and-filters
        (fn [[id shape]]
          (let [search (:search-text @filter-state)
                filters (:active-filters @filter-state)
                filters (cond-> filters
                          (some #{:shape} filters)
                          (conj :rect :circle :path :bool))]
            (or
             (= uuid/zero id)
             (and
              (or (str/includes? (str/lower (:name shape)) (str/lower search))
                  (str/includes? (dm/str (:id shape)) (str/lower search)))
              (or
               (empty? filters)
               (and
                (some #{:component} filters)
                (contains? shape :component-id))
               (let [direct_filters (filter #{:frame :rect :circle :path :bool :image :text} filters)]
                 (some #{(:type shape)} direct_filters))
               (and
                (some #{:group} filters)
                (and (= :group (:type shape))
                     (not (contains? shape :component-id))
                     (or (not (contains? shape :masked-group?)) (false? (:masked-group? shape)))))
               (and
                (some #{:mask} filters)
                (true? (:masked-group? shape))))))))

        filtered-objects-total
        (mf/use-memo
         (mf/deps objects active? @filter-state)
         #(when active?
            ;; filterv so count is constant time
            (filterv search-and-filters objects)))

        filtered-objects
        (mf/use-memo
         (mf/deps filtered-objects-total)
         #(when active?
            (calc-reparented-objects
             (into {}
                   (take (:num-items @filter-state))
                   filtered-objects-total))))

        handle-show-more
        (fn []
          (when (<= (:num-items @filter-state) (count filtered-objects-total))
            (swap! filter-state update :num-items + 100)))

        handle-key-down
        (mf/use-callback
         (fn [event]
           (let [enter? (kbd/enter? event)
                 esc?   (kbd/esc? event)
                 input-node (dom/event->target event)]

             (when enter?
               (dom/blur! input-node))
             (when esc?
               (dom/blur! input-node)))))]

    [filtered-objects
     handle-show-more
     (mf/html
      (if (:show-search-box @filter-state)
        [:*
         [:div {:class (dom/classnames (css :tool-window-bar) true
                                       (css :search) true)}
          [:span {:class (dom/classnames (css :search-box) true)}
           [:button
            {:on-click toggle-filters
             :class (dom/classnames :active active?
                                    (css :filter-button) true)}
            i/filter-refactor]
           [:div {:class (dom/classnames (css :search-input-wrapper) true)}
            [:input {:on-change update-search-text
                     :value (:search-text @filter-state)
                     :auto-focus (:show-search-box @filter-state)
                     :placeholder (tr "workspace.sidebar.layers.search")
                     :on-key-down handle-key-down}]
            (when (not (= "" (:search-text @filter-state)))
              [:button {:class (dom/classnames (css :clear) true)
                        :on-click clear-search-text} i/delete-text-refactor])]]
          [:button {:class (dom/classnames (css :close-search) true)
                    :on-click toggle-search} i/close-refactor]]
         [:div {:class (dom/classnames (css :active-filters) true)}
          (for [f (:active-filters @filter-state)]
            (let [name (case f
                         :frame (tr "workspace.sidebar.layers.frames")
                         :group (tr "workspace.sidebar.layers.groups")
                         :mask (tr "workspace.sidebar.layers.masks")
                         :component (tr "workspace.sidebar.layers.components")
                         :text (tr "workspace.sidebar.layers.texts")
                         :image (tr "workspace.sidebar.layers.images")
                         :shape (tr "workspace.sidebar.layers.shapes")
                         (tr f))]
              [:button {:class (dom/classnames (css :layer-filter) true)
                        :on-click (remove-filter f)}
               [:span {:class (dom/classnames (css :layer-filter-icon) true)}
                [:& sic/element-icon-refactor-by-type {:type f
                                                       :main-instance? (= f :component)}]]
               [:span {:class (dom/classnames (css :layer-filter-name) true)}
                name]
               [:span {:class (dom/classnames (css :layer-filter-close) true)}
                i/close-small-refactor]]))]

         (when (:show-filters-menu @filter-state)
           [:ul {:class (dom/classnames (css :filters-container) true)}
            [:li {:key "frames-filter-item"
                  :class (dom/classnames (css :filter-menu-item) true
                                         (css :selected) (contains? (:active-filters @filter-state) :frame))
                  :on-click (add-filter :frame)}
             [:div {:class (dom/classnames (css :filter-menu-item-name-wrapper) true)}
              [:span {:class (dom/classnames (css :filter-menu-item-icon) true)}
               i/board-refactor]
              [:span {:class (dom/classnames (css :filter-menu-item-name) true)}
               (tr "workspace.sidebar.layers.frames")]]
             (when (contains? (:active-filters @filter-state) :frame)
               [:span {:class (dom/classnames (css :filter-menu-item-tick) true)}
                i/tick-refactor])]
            [:li {:key "groups-filter-item"
                  :class (dom/classnames (css :filter-menu-item) true
                                         (css :selected) (contains? (:active-filters @filter-state) :group))
                  :on-click (add-filter :group)}
             [:div {:class (dom/classnames (css :filter-menu-item-name-wrapper) true)}
              [:span {:class (dom/classnames (css :filter-menu-item-icon) true)}
               i/group-refactor]
              [:span {:class (dom/classnames (css :filter-menu-item-name) true)}
               (tr "workspace.sidebar.layers.groups")]]
             (when (contains? (:active-filters @filter-state) :group)
               [:span {:class (dom/classnames (css :filter-menu-item-tick) true)}
                i/tick-refactor])]
            [:li {:key "masks-filter-item"
                  :class (dom/classnames (css :filter-menu-item) true
                                         (css :selected) (contains? (:active-filters @filter-state) :mask))
                  :on-click (add-filter :mask)}
             [:div {:class (dom/classnames (css :filter-menu-item-name-wrapper) true)}
              [:span {:class (dom/classnames (css :filter-menu-item-icon) true)}
               i/mask-refactor]
              [:span {:class (dom/classnames (css :filter-menu-item-name) true)}
               (tr "workspace.sidebar.layers.masks")]]
             (when (contains? (:active-filters @filter-state) :mask)
               [:span {:class (dom/classnames (css :filter-menu-item-tick) true)}
                i/tick-refactor])]
            [:li {:key "components-filter-item"
                  :class (dom/classnames (css :filter-menu-item) true
                                         (css :selected) (contains? (:active-filters @filter-state) :component))
                  :on-click (add-filter :component)}
             [:div {:class (dom/classnames (css :filter-menu-item-name-wrapper) true)}
              [:span {:class (dom/classnames (css :filter-menu-item-icon) true)}
               i/component-refactor]
              [:span {:class (dom/classnames (css :filter-menu-item-name) true)}
               (tr "workspace.sidebar.layers.components")]]
             (when (contains? (:active-filters @filter-state) :component)
               [:span {:class (dom/classnames (css :filter-menu-item-tick) true)}
                i/tick-refactor])]
            [:li {:key "texts-filter-item"
                  :class (dom/classnames (css :filter-menu-item) true
                                         (css :selected) (contains? (:active-filters @filter-state) :text))
                  :on-click (add-filter :text)}
             [:div {:class (dom/classnames (css :filter-menu-item-name-wrapper) true)}
              [:span {:class (dom/classnames (css :filter-menu-item-icon) true)}
               i/text-refactor]
              [:span {:class (dom/classnames (css :filter-menu-item-name) true)}
               (tr "workspace.sidebar.layers.texts")]]
             (when (contains? (:active-filters @filter-state) :text)
               [:span {:class (dom/classnames (css :filter-menu-item-tick) true)}
                i/tick-refactor])]
            [:li {:key "images-filter-item"
                  :class (dom/classnames (css :filter-menu-item) true
                                         (css :selected) (contains? (:active-filters @filter-state) :image))
                  :on-click (add-filter :image)}
             [:div {:class (dom/classnames (css :filter-menu-item-name-wrapper) true)}
              [:span {:class (dom/classnames (css :filter-menu-item-icon) true)}
               i/img-refactor]
              [:span {:class (dom/classnames (css :filter-menu-item-name) true)}
               (tr "workspace.sidebar.layers.images")]]
             (when (contains? (:active-filters @filter-state) :image)
               [:span {:class (dom/classnames (css :filter-menu-item-tick) true)}
                i/tick-refactor])]
            [:li {:key "shapes-filter-item"
                  :class (dom/classnames (css :filter-menu-item) true
                                         (css :selected) (contains? (:active-filters @filter-state) :shape))
                  :on-click (add-filter :shape)}
             [:div {:class (dom/classnames (css :filter-menu-item-name-wrapper) true)}
              [:span {:class (dom/classnames (css :filter-menu-item-icon) true)}
               i/path-refactor]
              [:span {:class (dom/classnames (css :filter-menu-item-name) true)}
               (tr "workspace.sidebar.layers.shapes")]]
             (when (contains? (:active-filters @filter-state) :shape)
               [:span {:class (dom/classnames (css :filter-menu-item-tick) true)}
                i/tick-refactor])]])]
        [:div {:class (dom/classnames (css :tool-window-bar) true)}
         [:span {:class (dom/classnames (css :page-name) true)}
          (:name page)]
         [:button {:class (dom/classnames (css :icon-search) true)
                   :on-click toggle-search} i/search-refactor]]))]))

(mf/defc layers-toolbox
  {:wrap [mf/memo]}
  [{:keys [size-parent] :as props}]
  (let [page  (mf/deref refs/workspace-page)
        focus (mf/deref refs/workspace-focus-selected)
        objects (hooks/with-focus-objects (:objects page) focus)
        title (when (= 1 (count focus)) (get-in objects [(first focus) :name]))

        observer-var (mf/use-var nil)
        lazy-load-ref (mf/use-ref nil)

        [filtered-objects show-more filter-component] (use-search page objects)

        intersection-callback
        (fn [entries]
          (when (and (.-isIntersecting (first entries)) (some? show-more))
            (show-more)))

        on-render-container
        (fn [element]
          (let [options #js {:root element}
                lazy-el (mf/ref-val lazy-load-ref)]
            (cond
              (and (some? element) (not (some? @observer-var)))
              (let [observer (js/IntersectionObserver. intersection-callback options)]
                (.observe observer lazy-el)
                (reset! observer-var observer))

              (and (nil? element) (some? @observer-var))
              (do (.disconnect @observer-var)
                  (reset! observer-var nil)))))

        on-scroll
        (fn [event]
          (let [target (dom/get-target event)
                target-top (:top (dom/get-bounding-rect target))
                frames (dom/get-elements-by-class "root-board")

                last-hidden-frame (->> frames
                                       (filter #(<= (- (:top (dom/get-bounding-rect %)) target-top) 0))
                                       last)
                frame-id (dom/get-attribute last-hidden-frame "id")
                children (dom/get-elements-by-class "sticky-children")
                last-hidden-children (->> children
                                          (filter #(< (- (:top (dom/get-bounding-rect %)) target-top) 0))
                                          last)
                is-children-shown? (> (- (:bottom (dom/get-bounding-rect last-hidden-children)) target-top) 0)

                children-frame-id (dom/get-attribute last-hidden-children "data-id")
                ;; We want to check that root-board is out of view but its children are not.
                ;; only in that case we make root board sticky.
                sticky? (and last-hidden-frame
                             is-children-shown?
                             (= frame-id children-frame-id))]
            (doseq [frame frames]
              (dom/remove-class! frame  "sticky"))

            (when sticky?
              (dom/add-class! last-hidden-frame "sticky"))))]
    [:div#layers
     {:class (dom/classnames (css :layers) true)}
     (if (d/not-empty? focus)
       [:div
        {:class (dom/classnames (css :tool-window-bar) true)}
        [:button {:class (dom/classnames (css :focus-title) true)
                  :on-click #(st/emit! (dw/toggle-focus-mode))}
         [:span {:class (dom/classnames (css :back-button-icon) true)} i/arrow-refactor]
         [:div {:class (dom/classnames (css :focus-name) true)} (tr "workspace.sidebar.layers")]
         [:div {:class (dom/classnames (css :focus-mode-tag-wrapper) true)}
          [:div {:class (dom/classnames (css :focus-mode-tag) true)} (tr "workspace.focus.focus-mode")]]]]
       filter-component)
     (if (some? filtered-objects)
       [:*
        [:div {:class (dom/classnames (css :tool-window-content) true)
               :ref on-render-container  :key "filters"}
         [:& filters-tree {:objects filtered-objects
                           :key (dm/str (:id page))
                           :parent-size size-parent}]
         [:div.lazy {:ref lazy-load-ref
                     :key "lazy-load"
                     :style {:min-height 16}}]]
        [:div {:on-scroll on-scroll
               :class (dom/classnames (css :tool-window-content) true)
               :style {:display (when (some? filtered-objects) "none")}}
         [:& layers-tree {:objects filtered-objects
                          :key (dm/str (:id page))
                          :filtered? true
                          :parent-size size-parent}]]]
       [:div {:on-scroll on-scroll
              :class (dom/classnames (css :tool-window-content) true)
              :style {:display (when (some? filtered-objects) "none")}}
        [:& layers-tree {:objects objects
                         :key (dm/str (:id page))
                         :filtered? false
                         :parent-size size-parent}]])]))
