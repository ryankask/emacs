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

;; For a publicly available version of the standards document, see:

;; http://www.loc.gov/standards/datetime/iso-tc154-wg5_n0038_iso_wd_8601-1_2016-02-16.pdf

;; The Wikipedia page on the standard is also informative:

;; https://en.wikipedia.org/wiki/ISO_8601

;;; Code:

(require 'time-date)
(require 'cl-lib)

(defun iso8601--concat-regexps (regexps)
  (mapconcat (lambda (regexp)
               (concat "\\(?:"
                       (replace-regexp-in-string "(" "(?:" regexp)
                       "\\)"))
             regexps "\\|"))

(defconst iso8601--year-match
  "\\([-+]\\)?\\([0-9][0-9][0-9][0-9]\\)")
(defconst iso8601--full-date-match
  "\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)")
(defconst iso8601--without-day-match
  "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)")
(defconst iso8601--outdated-date-match
  "--\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)")
(defconst iso8601--week-date-match
  "\\([0-9][0-9][0-9][0-9]\\)-?W\\([0-9][0-9]\\)-?\\([0-9]\\)?")
(defconst iso8601--ordinal-date-match
  "\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9][0-9]\\)")
(defconst iso8601--date-match
  (iso8601--concat-regexps
   (list iso8601--year-match
                   iso8601--full-date-match
                   iso8601--without-day-match
                   iso8601--outdated-date-match
                   iso8601--week-date-match
                   iso8601--ordinal-date-match)))

(defconst iso8601--time-match
  "\\([0-9][0-9]\\):?\\([0-9][0-9]\\)?:?\\([0-9][0-9]\\)?\\.?\\([0-9][0-9][0-9]\\)?")

(defconst iso8601--zone-match
  "\\(Z\\|\\([-+]\\)?\\([0-9][0-9]\\):?\\([0-9][0-9]\\)?\\)")

(defconst iso8601--combined-match
  (concat "\\(" iso8601--date-match "\\)"
          "\\(?:T\\("
          (replace-regexp-in-string "(" "(?:" iso8601--time-match)
          "\\)\\)?"
          "\\(" iso8601--zone-match "\\)?"))

(defconst iso8601--duration-full-match
  "P\\([0-9]+Y\\)?\\([0-9]+M\\)?\\([0-9]+D\\)?\\(T\\([0-9]+H\\)?\\([0-9]+M\\)?\\([0-9]+S\\)?\\)?")
(defconst iso8601--duration-week-match
  "P\\([0-9]+\\)W")
(defconst iso8601--duration-combined-match
  (concat "P" iso8601--combined-match))
(defconst iso8601--duration-match
  (iso8601--concat-regexps
   (list iso8601--duration-full-match
         iso8601--duration-week-match
         iso8601--duration-combined-match)))

(defun iso8601-parse (string)
  "Parse an ISO 8601 date/time string and return a `decoded-time' structure.

The ISO 8601 date/time strings look like \"2008-03-02T13:47:30\",
but shorter, incomplete strings like \"2008-03-02\" are valid, as
well as variants like \"2008W32\" (week number) and
\"2008-234\" (ordinal day number)."
  (if (not (iso8601-valid-p string))
      (signal 'wrong-type-argument string)
    (let* ((date-string (match-string 1 string))
           (time-string (match-string 2 string))
           (zone-string (match-string 3 string))
           (date (iso8601-parse-date date-string)))
      ;; The time portion is optional.
      (when time-string
        (let ((time (iso8601-parse-time time-string)))
          (setf (decoded-time-hour date) (decoded-time-hour time))
          (setf (decoded-time-minute date) (decoded-time-minute time))
          (setf (decoded-time-second date) (decoded-time-second time))))
      ;; The time zone is optional.
      (when zone-string
        (setf (decoded-time-zone date)
              ;; The time zone in decoded times are in seconds.
              (* (iso8601-parse-zone zone-string) 60)))
      date)))

(defun iso8601-parse-date (string)
  "Parse STRING (which should be on ISO 8601 format) and return a time value."
  (cond
   ;; Just a year: [-+]YYYY.
   ((iso8601--match iso8601--year-match string)
    (let ((year (string-to-number (match-string 2 string)))
          (sign (match-string 1 string)))
      (iso8601--decoded-time
       :year (if (string= sign "-")
                 ;; -0001 is 2 BCE.
                 (- year 1)
               year))))
   ;; Calendar dates: YYYY-MM-DD and variants.
   ((iso8601--match iso8601--full-date-match string)
    (iso8601--decoded-time
     :year (match-string 1 string)
     :month (match-string 2 string)
     :day (match-string 3 string)))
   ;; Calendar date without day: YYYY-MM.
   ((iso8601--match iso8601--without-day-match string)
    (iso8601--decoded-time
     :year (match-string 1 string)
     :month (match-string 2 string)))
   ;; Outdated date without year: --MM-DD
   ((iso8601--match iso8601--outdated-date-match string)
    (iso8601--decoded-time
     :month (match-string 1 string)
     :day (match-string 2 string)))
   ;; Week dates: YYYY-Www-D
   ((iso8601--match iso8601--week-date-match string)
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
   ((iso8601--match iso8601--ordinal-date-match string)
    (let* ((year (string-to-number (match-string 1 string)))
           (ordinal (string-to-number (match-string 2 string)))
           (month-day (date-ordinal-to-time year ordinal)))
      (iso8601--decoded-time :year year
                             :month (decoded-time-month month-day)
                             :day (decoded-time-day month-day))))
   (t
    (signal 'wrong-type-argument string))))

(defun iso8601-parse-time (string)
  "Parse STRING, which should be an ISO 8601 time string, and return a time value."
  (if (not (iso8601--match iso8601--time-match string))
      (signal 'wrong-type-argument string)
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
  (if (not (iso8601--match iso8601--zone-match string))
      (signal 'wrong-type-argument string)
    (if (match-string 2 string)
        ;; HH:MM-ish.
        (let ((hour (string-to-number (match-string 3 string)))
              (minute (and (match-string 4 string)
                           (string-to-number (match-string 4 string)))))
          (* (if (equal (match-string 2 string) "-")
                 -1
               1)
             (+ (* hour 60)
                (or minute 0))))
      ;; "Z".
      0)))

(defun iso8601-valid-p (string)
  "Say whether STRING is a valid ISO 8601 representation."
  (iso8601--match iso8601--combined-match string))

(defun iso8601-parse-duration (string)
  "Parse ISO 8601 durations on the form P3Y6M4DT12H30M5S."
  (cond
   ((and (iso8601--match iso8601--duration-full-match string)
         ;; Just a "P" isn't valid; there has to be at least one
         ;; element, like P1M.
         (> (length (match-string 0 string)) 2))
    (iso8601--decoded-time :year (or (match-string 1 string) 0)
                           :month (or (match-string 2 string) 0)
                           :day (or (match-string 3 string) 0)
                           :hour (or (match-string 5 string) 0)
                           :minute (or (match-string 6 string) 0)
                           :second (or (match-string 7 string) 0)))
   ;; PnW: Weeks.
   ((iso8601--match iso8601--duration-week-match string)
    (let ((weeks (string-to-number (match-string 1 string))))
      ;; Does this make sense?  Hm...
      (iso8601--decoded-time :day (* weeks 7))))
   ;; P<date>T<time>
   ((iso8601--match iso8601--duration-combined-match string)
    (iso8601-parse (substring string 1)))
   (t
    (signal 'wrong-type-argument string))))

(defun iso8601-parse-interval (string)
  "Parse ISO 8601 intervals."
  (let ((bits (split-string string "/"))
        start end duration)
    (if (not (= (length bits) 2))
        (signal 'wrong-type-argument string)
      ;; The intervals may be an explicit start/end times, or either a
      ;; start or an end, and an accompanying duration.
      (cond
       ((and (string-match "\\`P" (car bits))
             (iso8601-valid-p (cadr bits)))
        (setq duration (iso8601-parse-duration (car bits))
              end (iso8601-parse (cadr bits))))
       ((and (string-match "\\`P" (cadr bits))
             (iso8601-valid-p (car bits)))
        (setq duration (iso8601-parse-duration (cadr bits))
              start (iso8601-parse (car bits))))
       ((and (iso8601-valid-p (car bits))
             (iso8601-valid-p (cadr bits)))
        (setq start (iso8601-parse (car bits))
              end (iso8601-parse (cadr bits))))
       (t
        (signal 'wrong-type-argument string))))
    (unless end
      (setq end (decoded-time-add start duration)))
    (unless start
      (setq start (decoded-time-add end
                                    ;; We negate the duration so that
                                    ;; we get a subtraction.
                                    (mapcar (lambda (elem)
                                              (if (numberp elem)
                                                  (- elem)
                                                elem))
                                            duration))))
    (list start end
          (or duration
              (decode-time (time-subtract (encode-time end)
                                          (encode-time start))
                           (decoded-time-zone end))))))

(defun iso8601--match (regexp string)
  (string-match (concat "\\`" regexp "\\'") string))

(defun iso8601--value (elem default)
  (if (stringp elem)
      (string-to-number elem)
    (or elem default)))

(cl-defun iso8601--decoded-time (&key second minute hour
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

(provide 'iso8601)

;;; iso8601.el ends here
