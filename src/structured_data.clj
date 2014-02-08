(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
   (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[ax ay] [bx by]] rectangle]
    (- bx ax)))

(defn height [rectangle]
   (let [[[ax ay] [bx by]] rectangle]
    (- by ay)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
   (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[lx ly] [rx ry]] rectangle
        [px py] point]
    (and (>= px lx) (<= px rx) (>= py ly) (<= py ry))))

(defn contains-rectangle? [outer inner]
   (let [[p1 p2] inner]
   (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (book :authors)]
    (assoc book :authors  (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [take2nd (fn [x] (get x 1))]
    (map take2nd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (= (count a-seq) (set a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (-> (old-book->new-book book)
      (:authors)
      (contains? author)))

  ;(contains? (:authors (old-book->new-book book)) author))

(defn authors [books]
   (set (apply concat (map :authors books))))


(defn all-author-names [books]
    (set (map :name (authors books))))

;(defn all-author-names [books]
;  (let [author-names
;         (fn [book] (map :name (:authors book)))]
;    (set (apply concat (map author-names books)))))

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%


