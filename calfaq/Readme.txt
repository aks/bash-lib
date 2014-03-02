CALFAQ version 1.1, 4 April 2008
================================

COPYRIGHT
---------
  These functions are Copyright (c) 2008 by Claus Tondering
  (claus@tondering.dk).

  The "calfunc.c" program is Copyright (c) 2014 by Alan K. Stebbens
  <aks@stebbens.org>

LICENSE
-------
  The code is distributed under the Boost Software License, which says:

    Boost Software License - Version 1.0 - August 17th, 2003

    Permission is hereby granted, free of charge, to any person or
    organization obtaining a copy of the software and accompanying
    documentation covered by this license (the "Software") to use, reproduce,
    display, distribute, execute, and transmit the Software, and to prepare
    derivative works of the Software, and to permit third-parties to whom the
    Software is furnished to do so, all subject to the following:

    The copyright notices in the Software and this entire statement, including
    the above license grant, this restriction and the following disclaimer,
    must be included in all copies of the Software, in whole or in part, and
    all derivative works of the Software, unless such copies or derivative
    works are solely in the form of machine-executable object code generated
    by a source language processor.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT. IN NO EVENT
    SHALL THE COPYRIGHT HOLDERS OR ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE
    FOR ANY DAMAGES OR OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR
    OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
    USE OR OTHER DEALINGS IN THE SOFTWARE.

DESCRIPTION
-----------

  These functions are an implementation in the C language of the formulas
  presented in the Calendar FAQ at
  http://www.tondering.dk/claus/calendar.html.

  The documentation of each function is found in the .c and .h files.

  The implementation follows the formulas mentioned in version 2.9 of the FAQ
  quite closely. The focus of the implementation is on simplicity and clarity.
  For this reason, no complex data structures or classes are used, nor has any
  attempt been made to optimize the code. Also, no verification of the input
  parameters is performed (except in the function simple_gregorian_easter).

  All numbers (including Julian Day Numbers which current have values of
  almost 2,500,000) are assumed to be representable as variables of type
  'int'.

COMPATIBILITY
-------------
  All of this code has been successfully built and run using Microsoft Visual
  Studio .NET 2003.

  All of this code has been successfully built and run using GCC version
  3.4.4.

CHANGES SINCE VERSION 1.0
-------------------------
  The code of version 1.1 is exactly identical to version 1.0. The only
  difference is that the comments now refer to version 2.9 of the Calendar
  FAQ rather than version 2.8.
