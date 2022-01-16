//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef _LIBCPP_SUPPORT_NEWLIB_XLOCALE_H
#define _LIBCPP_SUPPORT_NEWLIB_XLOCALE_H

#if defined(_NEWLIB_VERSION)

#include <cstdlib>
#include <clocale>
#ifndef _LIBCPP_HAS_NO_WIDE_CHARACTERS
#include <cwctype>
#endif
#include <ctype.h>
#if ((!defined(__NEWLIB__) || __NEWLIB__ < 2 || \
   __NEWLIB__ == 2 && __NEWLIB_MINOR__ < 5) || \
   defined(_LIBCPP_HAS_LOCALIZATION_STUBS)) && \
   !(defined(__NEWLIB__) && (_POSIX_C_SOURCE < 200809))
#include <__support/xlocale/__nop_locale_mgmt.h>
#include <__support/xlocale/__posix_l_fallback.h>
#endif
#include <__support/xlocale/__strtonum_fallback.h>

#endif // _NEWLIB_VERSION

#endif
