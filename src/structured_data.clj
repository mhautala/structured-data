; Exercises from
; http://iloveponies.github.io/120-hour-epic-sax-marathon/structured-data.html

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

;

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



;

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
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))


;

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (-> (old-book->new-book book)
      (:authors)
      (contains? author)))


(defn authors [books]
   (set (apply concat (map :authors books))))


(defn all-author-names [books]
    (set (map :name (authors books))))


(defn author->string [author]
  (if (contains? author :birth-year)
    (str (author :name ) " (" (author :birth-year) " - " (author :death-year) ")")
    (str (author :name ))))


(defn authors->string [authors]
  (apply str
    (interpose ", "
      (map author->string authors))))


(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))


(defn books->string [books]
  (if (= 0 (count books))
    "No books."
    (apply str
      (concat (interpose ". " (map book->string books)) ["."]))))


(defn books-by-author [author books]
  (let [filt-author (fn [x] (has-author? x author))]
    (filter filt-author books)))


(defn author-by-name [name authors]
  (let [filt-name (fn [x] (= (:name x) name))]
    (first (filter filt-name authors))))


(defn living-authors [authors]
  (filter alive? authors))


(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))


(defn books-by-living-authors [books]
  (filter has-a-living-author? books))












; %________%


