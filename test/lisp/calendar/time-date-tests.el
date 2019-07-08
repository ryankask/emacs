;;; time-date-tests.el --- tests for calendar/time-date.el    -*- lexical-binding:t -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'time-date)

(ert-deftest test-leap-year ()
  (should-not (date-leap-year-p 1999))
  (should-not (date-leap-year-p 1900))
  (should (date-leap-year-p 2000))
  (should (date-leap-year-p 2004)))

(ert-deftest test-days-in-month ()
  (should (= (date-days-in-month 2004 2) 29))
  (should (= (date-days-in-month 2004 3) 31))
  (should-not (= (date-days-in-month 1900 3) 28)))

(ert-deftest test-ordinal ()
  (should (equal (time-ordinal-to-date 2008 271)
                 '(0 0 0 27 9 2008 nil nil nil)))
  (should (equal (time-ordinal-to-date 2008 1)
                 '(0 0 0 1 1 2008 nil nil nil)))
  (should (equal (time-ordinal-to-date 2008 32)
                 '(0 0 0 1 2 2008 nil nil nil)))
  (should (equal (time-ordinal-to-date 1981 095)
                 '(0 0 0 5 4 1981 nil nil nil))))

(require 'ert)

;;; time-date-tests.el ends here
