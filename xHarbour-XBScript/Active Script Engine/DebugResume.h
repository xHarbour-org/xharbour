/******************************************************************************
*
*  File: DebugResume.h
*
*  Author:  Joel Alley
*
*  Date: February 1, 1999
*
*  Description:   This file contains the declaration of a pure virtual class
*                 that allows the DebugErrorHandler in Active Debugging to 
*                 set the resume action for runtime errors.
*
*  Modifications:
******************************************************************************/

#ifndef _h_DebugResume
#define _h_DebugResume

class DebugResume
{
   public:
   virtual void SetResumeAction( long breakAction ){return;}
};
#endif
