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

(require 'time-date)

(defun iso8601--value (elem default)
  (if (stringp elem)
      (string-to-number elem)
    (or elem default)))

(cl-defmethod iso8601--decoded-time (&key second minute hour
                                          day month year
                                          dst zone)
  (list (iso8601--value second 0)
        (iso8601--value minute 0)
        (iso8601--value hour 0)
        (iso8601--value day 1)
        (iso8601--value month 1)
        (iso8601--value year 0)
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
      (iso8601--decoded-time
       :year (if (string= sign "-")
                 ;; -0001 is 2 BCE.
                 (- year 1)
               year))))
   ;; Calendar dates: YYY-MM-DD and variants.
   ((string-match "\\`\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)\\'"
                  string)
    (iso8601--decoded-time
     :year (match-string 1 string)
     :month (match-string 2 string)
     :day (match-string 3 string)))
   ;; Calendar date without day: YYYY-MM.
   ((string-match "\\`\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)\\'" string)
    (iso8601--decoded-time
     :year (match-string 1 string)
     :month (match-string 2 string)))
   ;; Outdated date without year: --MM-DD
   ((string-match "\\`--\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)\\'" string)
    (iso8601--decoded-time
     :month (match-string 1 string)
     :day (match-string 2 string)))
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
                         (iso8601--decoded-time :year year
                                                :month 1
                                                :day 4)))))
           (correction (+ (if (zerop jan-start) 7 jan-start)
                          3))
           (ordinal (+ (* week 7) (or day-of-week 0) (- correction))))
      (cond
       ;; Monday 29 December 2008 is written "2009-W01-1".
       ((< ordinal 1)
        (setq year (1- year)
              ordinal (+ ordinal (if (date-leap-year-p year)
                                     366 365))))
       ;; Sunday 3 January 2010 is written "2009-W53-7".
       ((> ordinal (if (date-leap-year-p year)
                       366 365))
        (setq ordinal (- ordinal (if (date-leap-year-p year)
                                     366 365))
              year (1+ year))))
      (let ((month-day (date-ordinal-to-time year ordinal)))
        (iso8601--decoded-time :year year
                               :month (decoded-time-month month-day)
                               :day (decoded-time-day month-day)))))
   ;; Ordinal dates: YYYY-DDD
   ((string-match "\\`\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9][0-9]\\)\\'"
                  string)
    (let* ((year (string-to-number (match-string 1 string)))
           (ordinal (string-to-number (match-string 2 string)))
           (month-day (date-ordinal-to-time year ordinal)))
      (iso8601--decoded-time :year year
                             :month (decoded-time-month month-day)
                             :day (decoded-time-day month-day))))))

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
      (iso8601--decoded-time :hour hour
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

(defun iso8601-parse-duration (string)
  "Parse ISO 8601 durations on the form P3Y6M4DT12H30M5S."
  (cond
   ((string-match "\\`P\\([0-9]+Y\\)?\\([0-9]+M\\)?\\([0-9]+D\\)?\\(T\\([0-9]+H\\)?\\([0-9]+M\\)?\\([0-9]+S\\)?\\)?\\'"
                  string)
    (when (> (length (match-string 0 string)) 2)
      (iso8601--decoded-time :year (or (match-string 1 string) 0)
                             :month (or (match-string 2 string) 0)
                             :day (or (match-string 3 string) 0)
                             :hour (or (match-string 5 string) 0)
                             :minute (or (match-string 6 string) 0)
                             :second (or (match-string 7 string) 0))))
   ;; PnW: Weeks.
   ((string-match "\\`P\\([0-9]+\\)W\\'" string)
    (let ((weeks (string-to-number (match-string 1 string))))
      ;; Does this make sense?  Hm...
      (iso8601--decoded-time :day (* weeks 7))))
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

(provide 'iso8601)

;;; iso8601.el ends here
