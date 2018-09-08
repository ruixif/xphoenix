*
**************************************************************
*
*  PHOENIX Version 2.1.4
*  Copyright (C) 2016 Wolfgang Sturhahn
*
*  This file is part of PHOENIX-2.1.4.
*
*  PHOENIX-2.1.4 is free software:
*  You can redistribute it and/or modify it under the terms
*  of the GNU General Public License as published by the
*  Free Software Foundation
*
*  PHOENIX-2.1.4 is distributed in the hope that it will be
*  useful, but WITHOUT ANY WARRANTY; without even the implied
*  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*  PURPOSE. See the GNU General Public License for details.
*
*  A copy of the GNU General Public License should have been
*  provided along with PHOENIX-2.1.4.
*  If not, see <http://www.gnu.org/licenses/>.
*
**************************************************************
*
       Subroutine CREAD(XLINE,XFORM,XSEP,XTYPE,R,D,I,X)
*
* purpose :  read data in a free format manner
* date    :  February 20, 1993
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    XLINE    = input line
*    XFORM    = ignored
*    XSEP     = separates comment and input field
*    XTYPE    = type of variable to read
*                R4  read variable type REAL*4
*                R8  read variable type REAL*8
*                I4  read variable type INTEGER*4
*                A   read variable type CHARACTER
*
* export variables :
*    R        = variable read as type REAL*4
*    D        = variable read as type REAL*8
*    I        = variable read as type INTEGER*4
*    X        = variable read as type CHARACTER*(*)
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      CHARACTER*(*) XLINE,XFORM,XSEP,XTYPE,X
      CHARACTER*72  XW(3),XPIN
*
*
*:  look for the separator
*
      IF(INDEX(XLINE,XSEP).NE.0) THEN
       XPIN = XLINE
       CALL XPARSE(XPIN,XSEP,3,XW)
*
*:  exception if first string in <XLINE> is separator
*
       IF(INDEX(XLINE,XSEP).EQ.1) XW(2) = XW(1)
*
       IF(XTYPE.EQ.'R4') READ(XW(2),*,ERR=999,END=999) R
       IF(XTYPE.EQ.'R8') READ(XW(2),*,ERR=999,END=999) D
       IF(XTYPE.EQ.'I4') READ(XW(2),*,ERR=999,END=999) I
       IF(XTYPE.EQ.'A ') THEN
        CALL XADJ(XW(2),' ')
        LX     = LEN(X)
        L      = MIN0(LX,LEN(XW(2)))
        X(1:L) = XW(2)(1:L)
        CALL XFILL(X,L+1,LX,' ')
       ENDIF
      ELSE
       IF(XTYPE.EQ.'R4') READ(XLINE,*,ERR=999,END=999) R
       IF(XTYPE.EQ.'R8') READ(XLINE,*,ERR=999,END=999) D
       IF(XTYPE.EQ.'I4') READ(XLINE,*,ERR=999,END=999) I
       IF(XTYPE.EQ.'A ') READ(XLINE,*,ERR=999,END=999) X
      ENDIF
*
      RETURN
*
*:  read errors
*
  999 CONTINUE
      WRITE(6,2000) XLINE
      IF(XTYPE.EQ.'R4') WRITE(6,2001)
      IF(XTYPE.EQ.'R8') WRITE(6,2002)
      IF(XTYPE.EQ.'I4') WRITE(6,2003)
      IF(XTYPE.EQ.'A ') WRITE(6,2004)
      R = 0.0
      D = 0.D0
      I = 0
      CALL XFILL(X,1,LEN(X),' ')
*
 2000 FORMAT(' ++ read error in the following line:'/
     +       '>',A72,'<')
 2001 FORMAT(' ++ reason :  real, single prec. expected..'/
     +       ' ++ parameter value is set to 0.0'/)
 2002 FORMAT(' ++ reason :  real, double prec. expected..'/
     +       ' ++ parameter value is set to 0.D0'/)
 2003 FORMAT(' ++ reason :  integer expected..'/
     +       ' ++ parameter value is set to 0'/)
 2004 FORMAT(' ++ reason :  character expected..'/
     +       ' ++ parameter value is set to blanks'/)
*
*
      END
