
(defvar leap-year 2020 "基準となる閏年")
(defvar standard-week '((3 . "水")) "基準年の1月1日の曜日")
(defvar week-code '((0 . "日") (1 . "月") (2 . "火") (3 . "水") (4 . "木") (5 . "金") (6 . "土")) "週をコード化した連想リスト")

(defun leaps (year) 
  "判定用として指定年までにある閏年の数と
  指定年が閏年の時に0となる数をresultリストで返す。
　戻り値(閏年の数 閏年の判定用)"
  (let ((diff (- year leap-year)) (result))
    (push (/ diff 4) result)
    (push (% diff 4) result) 
    (nreverse result)))


(setq long-manth '(1 3 5 7 8 10 12)) ;;普通の長さの月のリスト
(setq short-manth '(4 6 9 11)) ;;普通より短い月のリスト
(setq very-short-manth '(2)) ;;閏年の時に長さが変わり短い月より短い月

(defun days (year manth)
  "指定年の指定月の日のリストを返す"
  (let (stack '())
    (cond ((loop for x in long-manth thereis (= x manth)) 
	   (dotimes (i 31) 
	     (push (+ i 1) stack)))
	  ((loop for x in short-manth thereis (= x manth))
	   (dotimes (i 30) 
	     (push (+ i 1) stack)))
	  (t 
	   (if (= 0 (car (cdr (leaps year))))
	       (dotimes (i 29) 
		 (push (+ i 1) stack))
	     (dotimes (i 28) (push (+ i 1) stack))))) 
    (nreverse stack)))



(defun years-fast-week (year) ";;yearの1月1日の曜日コードを求めるただし今のところ紀元後だけ、紀元0年はないので紀元前の曜日が狂う"
  (let ((code 0) (diff (+ (* (car (leaps year)) 4) (car (cdr (leaps year))))))
    (if (< 0 diff)
	;; 移動量 -6から+6にする
	(while (<= 7 diff)
	  (setq diff (- diff 7)))
      (while (>= -7 diff)
	(setq diff (+ diff 7))))
    ;; ここで巡回群の処理
    (setq code (+ (car (car standard-week)) diff)) 
    (cond ((<= 7 code) (setq code (- code 7))) 
	  ((>= -1 code) (setq code (+ code 7)))
	  (t code))
    ))	  

(defun week (year manth day) "dayの曜日を求める"
  (let ((fast-week (years-fast-week year)) (sum 0))
	(dotimes (i (- manth 1)) 
	  (setq sum (+ sum (car (last (days year (+ i 1)))))))
	(setq sum (+ sum day))
	(while (<= 7 sum)
	  (setq sum (- sum 7)))
	))

(defun manth-days (year manth) "manthのカレンダーを出力"
  (let ((stack) (allday (days year manth)))
    (dolist (stack allday)
      (if (= 0 (car (assoc (week year manth stack) week-code)))
	  (princ (format "%d月%d日[%s]\n" manth stack
			 (cdr (assoc (week year manth stack) week-code))))
	(princ (format "%d月%d日(%s)\n" manth stack 
		       (cdr (assoc (week year manth stack) week-code))))))
    ))

;; カレンダー出力
(manth-days 2016 2)
