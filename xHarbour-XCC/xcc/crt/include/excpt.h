#ifndef _EXCPT_H
#define _EXCPT_H

/* excpt.h - private header for SEH definitions */

#define EXCEPTION_EXECUTE_HANDLER  1
#define EXCEPTION_CONTINUE_SEARCH  0
#define EXCEPTION_CONTINUE_EXECUTION  (-1)

typedef enum _EXCEPTION_DISPOSITION {
    ExceptionContinueExecution,
    ExceptionContinueSearch,
    ExceptionNestedException,
    ExceptionCollidedUnwind,
    ExceptionExecuteHandler
} EXCEPTION_DISPOSITION;

struct _EXCEPTION_RECORD;
struct _CONTEXT;

EXCEPTION_DISPOSITION __cdecl _except_handler(struct _EXCEPTION_RECORD *, void *, struct _CONTEXT *, void *);

/* declarations */
unsigned long __cdecl _exception_code(void);
void * __cdecl _exception_info(void);
int __cdecl _abnormal_termination(void);

/* macros */
#define GetExceptionCode  _exception_code
#define exception_code  _exception_code
#define GetExceptionInformation  (struct _EXCEPTION_POINTERS *)_exception_info
#define exception_info  (struct _EXCEPTION_POINTERS *)_exception_info
#define AbnormalTermination  _abnormal_termination
#define abnormal_termination  _abnormal_termination

#endif /* _EXCPT_H */
