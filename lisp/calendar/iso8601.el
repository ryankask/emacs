;;; iso8601.el --- parse ISO 8601 date/time strings  -*- lexical-binding:t -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Keywords: dates

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(cl-defmethod make-decoded-time (&key second minute hour
                                      day month year
                                      dst zone)
  (list (or second 0)
        (or minute 0)
        (or hour 0)
        (or day 1)
        (or month 1)
        (or year 0)
        nil
        dst
        zone))

(defun iso8601-parse-date (string)
  "Parse STRING (which should be on ISO 8601 format) and return a time value."
  (cond
   ;; Just a year: [-+]YYYY.
   ((string-match "\\`\\([-+]\\)?\\([0-9][0-9][0-9][0-9]\\)\\'" string)
    (let ((year (string-to-number (match-string 2 string)))
          (sign (match-string 1 string)))
      (make-decoded-time
       :year (if (string= sign "-")
                 ;; -0001 is 2 BCE.
                 (- year 1)
               year))))
   ;; Calendar dates: YYY-MM-DD and variants.
   ((string-match "\\`\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)\\'"
                  string)
    (make-decoded-time
     :year (string-to-number (match-string 1 string))
     :month (string-to-number (match-string 2 string))
     :day (string-to-number (match-string 3 string))))
   ;; Calendar date without day: YYYY-MM.
   ((string-match "\\`\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)\\'" string)
    (make-decoded-time
     :year (string-to-number (match-string 1 string))
     :month (string-to-number (match-string 2 string))))
   ;; Outdated date without year: --MM-DD
   ((string-match "\\`--\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)\\'" string)
    (make-decoded-time
     :month (string-to-number (match-string 1 string))
     :day (string-to-number (match-string 2 string))))
   ;; Week dates: YYYY-Www-D
   ((string-match "\\`\\([0-9][0-9][0-9][0-9]\\)-?W\\([0-9][0-9]\\)-?\\([0-9]\\)?\\'"
                  string)
    (let* ((year (string-to-number (match-string 1 string)))
           (week (string-to-number (match-string 2 string)))
           (day-of-week (and (match-string 3 string)
                             (string-to-number (match-string 3 string))))
           (jan-start (decoded-time-weekday
                       (decode-time
                        (encode-time
                         (make-decoded-time :year year
                                            :month 1
                                            :day 4)))))
           (correction (+ (if (zerop jan-start) 7 jan-start)
                          3))
           (ordinal (+ (* week 7) (or day-of-week 0) (- correction))))
      (cond
       ;; Monday 29 December 2008 is written "2009-W01-1".
       ((< ordinal 1)
        (setq year (1- year)
              ordinal (+ ordinal (iso8601-days-in-year year))))
       ;; Sunday 3 January 2010 is written "2009-W53-7".
       ((> ordinal (iso8601-days-in-year year))
        (setq ordinal (- ordinal (iso8601-days-in-year year))
              year (1+ year))))
      (let ((month-day (iso8601-ordinal-to-date year ordinal)))
        (make-decoded-time :year year
                           :month (car month-day)
                           :day (cdr month-day)))))
   ;; Ordinal dates: YYYY-DDD
   ((string-match "\\`\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9][0-9]\\)\\'"
                  string)
    (let* ((year (string-to-number (match-string 1 string)))
           (ordinal (string-to-number (match-string 2 string)))
           (month-day (iso8601-ordinal-to-date year ordinal)))
      (make-decoded-time :year year
                         :month (car month-day)
                         :day (cdr month-day))))))

(defun iso8601-parse-time (string)
  "Parse STRING, which should be an ISO 8601 time string, and return a time value."
  (when (string-match "\\`\\([0-9][0-9]\\):?\\([0-9][0-9]\\)?:?\\([0-9][0-9]\\)?\\.?\\([0-9][0-9][0-9]\\)?\\'"
                      string)
    (let ((hour (string-to-number (match-string 1 string)))
          (minute (and (match-string 2 string)
                       (string-to-number (match-string 2 string))))
          (second (and (match-string 3 string)
                       (string-to-number (match-string 3 string))))
          ;; Hm...
          (_millisecond (and (match-string 4 string)
                             (string-to-number (match-string 4 string)))))
      (make-decoded-time :hour hour
                         :minute (or minute 0)
                         :second (or second 0)))))

(defun iso8601-parse-zone (string)
  "Parse STRING, which should be an ISO 8601 time zone.
Return the number of minutes."
  (when (string-match "\\`\\(Z\\|\\([-+]\\)?\\([0-9][0-9]\\):?\\([0-9][0-9]\\)?\\)\\'"
                      string)
    (if (match-string 1 string)
        ;; HH:MM-ish.
        (let ((hour (string-to-number (match-string 3 string)))
              (minute (and (match-string 4 string)
                           (string-to-number (match-string 5 string)))))
          (+ (* (if (equal (match-string 1 string) "-")
                    (- hour)
                  hour)
                60
                (or minute 0))))
      ;; "Z".
      0)))

(defun iso8601-parse (string)
  "Parse a time string."
  (when (string-match "\\`\\([^T]+\\)\\(T\\([:0-9]+\\)\\(.+\\)?\\)?\\'" string)
    (let* ((date-string (match-string 1 string))
           (time-string (match-string 3 string))
           (zone-string (match-string 4 string))
           (date (iso8601-parse-date date-string)))
      (when time-string
        (let ((time (iso8601-parse-time time-string)))
          (setf (decoded-time-hour date) (decoded-time-hour time))
          (setf (decoded-time-minute date) (decoded-time-minute time))
          (setf (decoded-time-second date) (decoded-time-second time))))
      (when zone-string
        (setf (decoded-time-zone date)
              ;; The time zone in decoded times are in seconds.
              (* (iso8601-parse-zone zone-string) 60)))
      date)))

(defun iso8601-days-in-year (year)
  (if (and (zerop (% year 4))
           (if (zerop (% year 100))
               (not (zerop (% year 400)))
             t))
      366
    365))

(defun iso8601-days-in-month (year month)
  (if (= month 2)
      (if (= (iso8601-days-in-year year) 365)
          28
        29)
    (if (memq month '(1 3 5 7 8 10 12))
        31
     30)))

(defun iso8601-ordinal-to-date (year ordinal)
  (let ((month 1))
    (while (> ordinal (iso8601-days-in-month year month))
      (setq ordinal (- ordinal (iso8601-days-in-month year month))
            month (1+ month)))
    (cons month ordinal)))

(defun iso8601-parse-duration (string)
  "Parse ISO 8601 durations on the form P3Y6M4DT12H30M5S."
  (cond
   ((string-match "\\`P\\([0-9]+Y\\)?\\([0-9]+M\\)?\\([0-9]+D\\)?\\(T\\([0-9]+H\\)?\\([0-9]+M\\)?\\([0-9]+S\\)?\\)?\\'"
                  string)
    (let ((year (match-string 1 string))
          (month (match-string 2 string))
          (day (match-string 3 string))
          (hour (match-string 5 string))
          (minute (match-string 6 string))
          (second (match-string 7 string)))
      (when (> (length (match-string 0 string)) 2)
        (make-decoded-time :year (if year (string-to-number year) 0)
                           :month (if month (string-to-number month) 0)
                           :day (if day (string-to-number day) 0)
                           :hour (if hour (string-to-number hour) 0)
                           :minute (if minute (string-to-number minute) 0)
                           :second (if second (string-to-number second) 0)))))
   ;; PnW: Weeks.
   ((string-match "\\`P\\([0-9]+\\)W\\'" string)
    (let ((weeks (string-to-number (match-string 1 string))))
      ;; Does this make sense?  Hm...
      (make-decoded-time :day (* weeks 7))))
   ;; P<date>T<time>
   ((string-match "\\`P[-0-9W]+T[:0-9]+\\'" string)
    (iso8601-parse (substring string 1)))))

(defun iso8601-parse-interval (string)
  "Parse ISO 8601 intervals."
  (let ((bits (split-string string "/"))
        start end duration)
    (when (= (length bits) 2)
      (cond
       ((string-match "\\`P" (car bits))
        (setq duration (iso8601-parse-duration (car bits))
              end (encode-time (iso8601-parse (cadr bits)))
              start (time-subtract end (encode-time duration))))
       ((string-match "\\`P" (cadr bits))
        (setq duration (iso8601-parse-duration (cadr bits))
              start (encode-time (iso8601-parse (car bits)))
              end (time-add start (encode-time duration))))
       (t
        (setq start (encode-time (iso8601-parse (car bits)))
              end (encode-time (iso8601-parse (cadr bits)))))))
    (list start end)))


;; (format-time-string "%FT%T" (iso8601-parse-date "1985"))
;; "1985-01-01T00:00:00"

;; (format-time-string "%FT%T" (iso8601-parse-date "-0003"))
;; "0002-01-01T00:00:00"

;; (format-time-string "%FT%T" (iso8601-parse-date "+1985"))
;; "1985-01-01T00:00:00"

;; (format-time-string "%FT%T" (iso8601-parse-date "1985-03-14"))
;; "1985-03-14T00:00:00"

;; (format-time-string "%FT%T" (iso8601-parse-date "19850314"))
;; "1985-03-14T00:00:00"

;; (format-time-string "%FT%T" (iso8601-parse-date "1985-02"))
;; "1985-02-01T00:00:00"

;; (format-time-string "%FT%T" (iso8601-parse-date "--02-01"))
;; "0000-02-01T00:00:00"

;; (format-time-string "%FT%T" (iso8601-parse-date "--0201"))
;; "0000-02-01T00:00:00"

;; (iso8601-days-in-year 1999)
;; 365

;; (iso8601-days-in-year 1900)
;; 366

;; (iso8601-days-in-year 1996)
;; 366

;; (iso8601-days-in-year 2000)
;; 365

;; (iso8601-days-in-month 2001 5)
;; 31

;; (iso8601-days-in-month 2004 2)
;; 29

;; (iso8601-days-in-month 2001 11)
;; 30

;; (iso8601-ordinal-to-date 2008 271)
;; (9 . 27)

;; (iso8601-ordinal-to-date 2008 1)
;; (1 . 1)

;; (iso8601-ordinal-to-date 2008 32)
;; (2 . 1)

;; (iso8601-ordinal-to-date 1981 095)
;; (4 . 5)

;; (format-time-string "%FT%T" (iso8601-parse-date "2008W39-6"))
;; "2008-09-27T01:00:00"

;; (format-time-string "%FT%T" (iso8601-parse-date "2009W01-1"))
;; "2008-12-29T00:00:00"

;; (format-time-string "%FT%T" (iso8601-parse-date "2009W53-7"))
;; "2010-01-03T00:00:00"

;; (format-time-string "%FT%T" (iso8601-parse-date "1981-095"))
;; "1981-04-05T01:00:00"

;; (format-time-string "%G-W%V-%u" (encode-time '(0 0 0 29 12 2008 nil nil nil)))
;; "2009-W01-1"

;; (format-time-string "%FT%T" (iso8601-parse-date "2009W01-1"))
;; "2009-01-05T00:00:00"

;; (format-time-string "%FT%T" (encode-time (iso8601-parse-time "13:47:30")))
;; "0000-01-01T13:47:30"

;; (format-time-string "%FT%T" (encode-time (iso8601-parse "2008-03-02T13:47:30")))
;; "2008-03-02T13:47:30"


;; (iso8601-parse-duration "P3Y6M4DT12H30M5S")
;; (5 30 12 4 6 3 nil nil nil)

;; (iso8601-parse-duration "P1M")
;; (0 0 0 0 1 0 nil nil nil)

;; (iso8601-parse-duration "PT1M")
;; (0 1 0 0 0 0 nil nil nil)

;; (iso8601-parse-duration "P0003-06-04T12:30:05")
(5 30 12 4 6 3 nil nil nil)


(provide 'iso8601)

;;; iso8601.el ends here
