/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (UA1125)
 *
 * Copyright 2009 Pavel Tsarenko <tpe2@mail.ru>
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* Language name: Ukrainian */
/* ISO language code (2 chars): UA */
/* Codepage: 1125 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "UA1125",                    /* ID */
      "Ukrainian",                 /* Name (in English) */
      "Укра∙нська",                /* Name (in native language) */
      "UA",                        /* RFC ID */
      "1125",                      /* Codepage */
      "$Revision$ $Date$",         /* Version */

      /* Month names */

      "Сўчень",
      "Лютий",
      "Березень",
      "Квўтень",
      "Травень",
      "Червень",
      "Липень",
      "Серпень",
      "Вересень",
      "Жовтень",
      "Листопад",
      "Грудень",

      /* Day names */

      "Недўля",
      "Понедўлок",
      "Вўвторок",
      "Середа",
      "Четвер",
      "П'ятниця",
      "Субота",

      /* CA-Cl*pper compatible natmsg items */

      "Файли даних       # Записи     Остання зм.     Розмўр",
      "Потрўбнў ще приклади ?",
      "Стор.N",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Вст",
      "   ",
      "Помилкова дата",
      "Дўапазон: ",
      " - ",
      "Т/Н",
      "ПОМИЛКОВИЙ ВИРАЗ",

      /* Error description names */

      "Невўдома помилка",
      "Помилковий аргумент",
      "Переповнення масиву",
      "Переповнення рядка",
      "Переповнення числа",
      "Дўлення на нуль",
      "Числова помилка",
      "Синтаксична помилка",
      "Дуже складна операцўя",
      "",
      "",
      "Не вистачаї пам'ятў",
      "Невўдома функцўя",
      "Метод не експортований",
      "Змўнна не ўснуї",
      "Алўас не ўснуї",
      "Змўнна не експортована",
      "Недопустимў символи у ўменў алўаса",
      "Алўас вже використовуїться",
      "",
      "Помилка пўд час створення",
      "Помилка пўд час вўдкриття",
      "Помилка пўд час закриття",
      "Помилка пўд час читання",
      "Помилка пўд час запису",
      "Помилка пўд час друку",
      "",
      "",
      "",
      "",
      "Операцўя не пўдтримуїться",
      "Лўмўт перевищено",
      "Виявлено пошкодження",
      "Помилка в типў даних",
      "Помилка в розмўрў даних",
      "Файл не вўдкритий",
      "Файл не проўндексований",
      "Потрўбен ексклюзивний доступ",
      "Потрўбне блокування",
      "Запис заборонено",
      "Збiй блокування пўд час додавання запису",
      "заблокувати не вдалося",
      "",
      "",
      "",
      "",
      "Помилкова кўлькўсть аргументўв",
      "Доступ до масиву",
      "присвоювання масиву",
      "не масив",
      "порўвняння",
      "Invalid self",


      /* Internal error names */

      "Невиправна помилка %lu: ",
      "Помилка пўд час вўдновлення",
      "Не визначено ERRORBLOCK() для помилки",
      "Перевищена межа рекурсивних викликўв обробника помилок",
      "Не вдаїться завантажити RDD",
      "Помилковий тип методу %s",
      "hb_xgrab не може розподўлити пам'ять",
      "hb_xrealloc викликано з NULL вказўвником",
      "hb_xrealloc викликано з помилковим вказўвником",
      "hb_xrealloc не може перерозподўлити пам'ять",
      "hb_xfree викликано с помилковим вказўвником",
      "hb_xfree викликано з NULL вказўвником",
      "Не знайдена стартова процедура: \'%s\'",
      "Вўдсутня стартова процедура",
      "VM: Невўдомий код",
      "%s: очўкувався символ",
      "%s: помилковий тип символу для self",
      "%s: очўкувався блок коду",
      "%s: помилковий тип елементу на вершинў стеку",
      "Вихўд за межў стеку",
      "%s: спроба копўювати елемент на себе",
      "%s: помилкове ўм'я змўнно∙",
      "Переповнення буферу пам'ятў",
      "hb_xgrab requested to allocate zero bytes",
      "hb_xrealloc requested to resize to zero bytes",
      "hb_xalloc requested to allocate zero bytes",

      /* Texts */

      "РРРР/ММ/ДД",
      "Т",
      "Н"
   }
};

#define HB_LANG_ID  UA1125
#include "hbmsgreg.h"
