;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns uxbox.main.ui.workspace.shapes.interactions
  "Visually show shape interactions in workspace"
  (:require
   [rumext.alpha :as mf]
   [cuerdas.core :as str]
   [uxbox.util.data :as dt]
   [uxbox.util.dom :as dom]
   [uxbox.util.geom.point :as gpt]
   [uxbox.util.geom.shapes :as geom]
   [uxbox.main.store :as st]
   [uxbox.main.refs :as refs]
   [uxbox.main.data.workspace :as dw]
   [uxbox.main.ui.keyboard :as kbd]
   ))

(defn- get-click-interaction
  [shape]
  (first (filter #(= (:event-type %) :click) (:interactions shape))))


(defn- on-mouse-down
  [event {:keys [id type] :as shape} selected]
  (do
    (dom/stop-propagation event)
    (when-not (empty? selected)
      (st/emit! dw/deselect-all))
    (st/emit! (dw/select-shape id))
    (st/emit! (dw/start-create-interaction))))


(defn connect-to-shape
  "Calculate the best position to draw an interaction line
  between two shapes"
  [orig-shape dest-shape]
  (let [orig-rect (geom/selection-rect-shape orig-shape)
        dest-rect (geom/selection-rect-shape dest-shape)

        orig-x-left (:x orig-rect)
        orig-x-right (+ orig-x-left (:width orig-rect))
        orig-x-center (+ orig-x-left (/ (:width orig-rect) 2))

        dest-x-left (:x dest-rect)
        dest-x-right (+ dest-x-left (:width dest-rect))
        dest-x-center (+ dest-x-left (/ (:width dest-rect) 2))

        orig-pos (if (<= orig-x-right dest-x-left) :right
                   (if (>= orig-x-left dest-x-right) :left
                     (if (<= orig-x-center dest-x-center) :left :right)))
        dest-pos (if (<= orig-x-right dest-x-left) :left
                   (if (>= orig-x-left dest-x-right) :right
                     (if (<= orig-x-center dest-x-center) :left :right)))

        orig-x (if (= orig-pos :right) orig-x-right orig-x-left)
        dest-x (if (= dest-pos :right) dest-x-right dest-x-left)

        orig-y (+ (:y orig-rect) (/ (:height orig-rect) 2))
        dest-y (+ (:y dest-rect) (/ (:height dest-rect) 2))]

    [orig-pos orig-x orig-y dest-pos dest-x dest-y]))


(defn connect-to-point
  "Calculate the best position to draw an interaction line
  between one shape and one point"
  [orig-shape dest-point]
  (let [orig-rect (geom/selection-rect-shape orig-shape)

        orig-x-left (:x orig-rect)
        orig-x-right (+ orig-x-left (:width orig-rect))
        orig-x-center (+ orig-x-left (/ (:width orig-rect) 2))

        dest-x (:x dest-point)
        dest-y (:y dest-point)

        orig-pos (if (<= orig-x-right dest-x) :right
                   (if (>= orig-x-left dest-x) :left
                     (if (<= orig-x-center dest-x) :right :left)))
        dest-pos (if (<= orig-x-right dest-x) :left
                   (if (>= orig-x-left dest-x) :right
                     (if (<= orig-x-center dest-x) :right :left)))

        orig-x (if (= orig-pos :right) orig-x-right orig-x-left)
        orig-y (+ (:y orig-rect) (/ (:height orig-rect) 2))]

    [orig-pos orig-x orig-y dest-pos dest-x dest-y]))


(mf/defc interaction-path
  [{:keys [orig-shape dest-shape dest-point selected selected?] :as props}]
  (let [[orig-pos orig-x orig-y dest-pos dest-x dest-y]
        (if dest-shape
          (connect-to-shape orig-shape dest-shape)
          (connect-to-point orig-shape dest-point))

        orig-dx (if (= orig-pos :right) 100 -100)
        dest-dx (if (= dest-pos :right) 100 -100)

        path ["M" orig-x orig-y "C" (+ orig-x orig-dx) orig-y (+ dest-x dest-dx) dest-y dest-x dest-y]
        pdata (str/join " " path)

        arrow-path (if (= dest-pos :left)
                     ["M" (- dest-x 5) dest-y "l 8 0 l -4 -4 m 4 4 l -4 4"]
                     ["M" (+ dest-x 5) dest-y "l -8 0 l 4 -4 m -4 4 l 4 4"])
        arrow-pdata (str/join " " arrow-path)]

    (if-not selected?
      [:path {:stroke "#B1B2B5"
              :fill "transparent"
              :stroke-width 2
              :d pdata
              :on-mouse-down #(on-mouse-down % orig-shape selected)}]

      [:g {:on-mouse-down #(on-mouse-down % orig-shape selected)}
       [:path {:stroke "#31EFB8"
               :fill "transparent"
               :stroke-width 2
               :d pdata}]
       [:circle {:cx orig-x
                 :cy orig-y
                 :r 8
                 :stroke "#31EFB8"
                 :stroke-width 2
                 :fill "#FFFFFF"}]
       [:circle {:cx dest-x
                 :cy dest-y
                 :r 8
                 :stroke "#31EFB8"
                 :stroke-width 2
                 :fill "#FFFFFF"}]
       [:path {:stroke "#31EFB8"
               :fill "transparent"
               :stroke-width 2
               :d arrow-pdata}]])))


(mf/defc interaction-handle
  [{:keys [shape selected] :as props}]
  (let [shape-rect (geom/selection-rect-shape shape)
        handle-x (+ (:x shape-rect) (:width shape-rect))
        handle-y (+ (:y shape-rect) (/ (:height shape-rect) 2))

        arrow-path ["M" (- handle-x 5) handle-y "l 8 0 l -4 -4 m 4 4 l -4 4"]
        arrow-pdata (str/join " " arrow-path)]

    [:g {:on-mouse-down #(on-mouse-down % shape selected)}
       [:circle {:cx handle-x
                 :cy handle-y
                 :r 8
                 :stroke "#31EFB8"
                 :stroke-width 2
                 :fill "#FFFFFF"}]
       [:path {:stroke "#31EFB8"
               :fill "transparent"
               :stroke-width 2
               :d arrow-pdata}]]))


(mf/defc interactions
  [{:keys [selected] :as props}]
  (let [data (mf/deref refs/workspace-data)
        local (mf/deref refs/workspace-local)
        current-transform (:transform local)
        objects (:objects data)
        active-shapes (filter #(first (get-click-interaction %)) (vals objects))
        selected-shapes (map #(get objects %) selected)
        draw-interaction-to (:draw-interaction-to local)
        first-selected (first selected-shapes)]
    [:*
      (for [shape active-shapes]
        (let [interaction (get-click-interaction shape)
              dest-shape (get objects (:destination interaction))
              selected? (contains? selected (:id shape))]
          (when-not selected?
            [:& interaction-path {:key (:id shape)
                                  :orig-shape shape
                                  :dest-shape dest-shape
                                  :selected selected
                                  :selected? false}])))

      (if (and draw-interaction-to first-selected)
        [:& interaction-path {:key "interactive"
                              :orig-shape first-selected
                              :dest-point draw-interaction-to
                              :selected? true}]

        (for [shape selected-shapes]
          (let [interaction (get-click-interaction shape)
                dest-shape (get objects (:destination interaction))]
            (if dest-shape
              [:& interaction-path {:key (:id shape)
                                    :orig-shape shape
                                    :dest-shape dest-shape
                                    :selected selected
                                    :selected? true}]
              (when (not (#{:move :rotate} current-transform))
                [:& interaction-handle {:key (:id shape)
                                        :shape shape
                                        :selected selected}])))))]))
