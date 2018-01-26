;;定義済みのシンボル
;;comp :関数名
;;get-src-string :関数名 ファイル読み込み 戻り値 source-string
;;split-record  :関数名 データを改行でわけて1レコードとする。 戻り値 record
;;get-record :関数名 レコードデータをリスト形式に変換　戻り値 output
;;record-reader :関数名
;;csv-read :関数名 csvに変換するユニットのメイン 戻り値 output

;;filename  :グローバル->csv-lead
;;output :グローバル->record-to-list->get-record
;;source-string get-src-string:グローバル
;;pre-buffer get-src-string:ローカル
;;stack  csv-lead:ローカル->record-to-list
;;data split-record:引数->get-record:引数
;;filename  get-src-string:引数
;;record record-to-list:引数

;;実行
;;csv-lead->get-src-string
;;csv-lead->split-record->get-record->record-to-list

(defvar filename "/Volumes/artbox.xnet/ftp/incoming/data.csv" "Open File name")
(defvar source-string "" "Input data")
(defvar output nil "Output data")

(defun comp (element) "変換機" 
  ;; 要素である文字列の先頭を検査する
  (if (not (= (length element) 0))
      (if (string-match "[1-9]" (substring element 0 1)) 
          ;; 要素を数に変換する
          (string-to-number element)
        ;; そのままにする
        element)
    element))


(defun record-reader (record) "レコードの内容を一つずつ読み込み変換器に渡す"
;;レコードから要素を一つとる
  (push (pop record) stack)
;;変換器に渡して変換してスタックに戻す
  (push (comp (pop stack)) stack)
;;レコード要素が無くなるまで繰り返す
  (if (not (null record)) 
      (record-reader record)
;;要素がなくなったら変換したリストを返す
    (prog1
      (push (nreverse stack) output)  ;; 戻り値
      (setq stack '()))))

(defun get-record (data) "カンマ区切りのレコードをリストに変換する"
;;1レコード読み込み　,　を区切りにリストの各要素に分けてリーダーに渡す
  (record-reader (split-string (first data) ","))
;;レコードデータから1レコード削除する
  (pop data)
;;データが無くなるまで繰り返し、無くなったら変換したリストを返す
  (if (not (null data))
      (get-record data)
    (setq output (nreverse output)))) ;;　戻り値

(defun split-record (data) "csvデータを改行で分けてそれぞれ1レコードのデータにする" 
  (setq data (split-string data "\n")))

(defun get-src-string (filename) "ファイル読み込み"
  (let 
;; カレントバッファをpre-bufferに設定
      ((pre-buffer (current-buffer)))
;; ファイルを開く 
    (find-file filename) 
    (setq source-string 
;;バッファの文字列を読み込み変数に設定
          (buffer-substring-no-properties (point-min) (point-max)))
;; カレントバッファに戻し文字列を返す 
    (switch-to-buffer pre-buffer)
    source-string)) ;; 戻り値

(defun clear-messages ()
  (let
      ((message-buffer (get-buffer "*Messages*"))
       (input-buffer (current-buffer)))
    (set-buffer message-buffer)
    (erase-buffer)
    (switch-to-buffer input-buffer)))


(defun csv-read (filename) "csvデータを読み込むメイン"
  (let ((stack '()))
    (setq output '())
    (get-record (split-record (get-src-string filename)))))




(defun csv-write (filename　&option output) "csvデータ出力"
  (clear-messages) ;;出力先バッファクリア
  (let
      ((pre-buffer (current-buffer)) ;;バッファ退避用
       (stack '()) ;; データ処理用
       (head "")   ;; レコードの先頭要素
       (str "") ;; レコードの要素
       (pre_str "")) ;;前の要素
    ;;バッファを変える
    (set-buffer (get-buffer "*Messages*"))
    ;; null値でないないならレコード取得を繰り返す
    (while (not (null (setq stack (pop output))))
;;       ;;レコードの先頭要素を取り出す
      (when (not (null (setq head (pop stack))))
;;       ;; 要素の種類判別
        (cond ((stringp head) (setq pre_str head)) 
              ((numberp head) (setq pre_str (number-to-string head)))
              ((t (setq pre_str ""))))
;;
        (while (not (null (setq str (pop stack))))
;;         ;; 最後の一つ前の要素までを取り出す
          (while (and (/= (length stack) 1) (/= (length stack) 0))
;;             ;; 要素の種類判別
            (cond ((stringp str) (setq pre_str (concat pre_str "," str)))
                  ((numberp str) (setq pre_str (concat pre_str "," (number-to-string str))))
                  ((t (setq pre_str (concat pre_str "," "")))))
            (setq str (pop stack)))
          ;; 最後の要素
          (cond ((stringp str) (setq pre_str (concat pre_str "," str)))
                ((numberp str) (setq pre_str (concat pre_str "," (number-to-string str))))
                ((t (setq pre_str (concat pre_str "," ""))))))      
       (message pre_str)))
    ;; 出力
    ;;バッファを戻す
    (switch-to-buffer pre-buffer)))

(csv-read filename)
(csv-write filename output)





















































































 




























































































































