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
      Subroutine BGCORR(NERR)
*
* purpose :  correct measured data for
*              - constant background;
*              - linear normalization errors;
*              - linear and quadratic energy scale errors.
* date    :  March 24, 2012
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    KD         = no. of data points
*    DMX(*)     = energy scale
*    DMY(*)     = counts
*    XDBG(*)    = background/normalization information
*
* export variables :
*    NERR       = error code
*    DMX(*)     = corrected energy scale
*    DMY(*)     = counts minus background and normalization corrected
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<phox.h>
*
      Character*72 XP(6)
*
*
*:  reset error code
*
      NERR = 0
*
*:  investigate options
*
      CALL XPARSE(XDBG,' ',6,XP)
      CALL XTOUP(XP(1))
      IF(XP(1)(1:4).EQ.'AUTO') THEN
*
*:  determine background from first data points
*
       READ(XP(2),*,ERR=999) NAVPTS
       IF(NAVPTS.LE.0 .OR. NAVPTS.GT.KD) Then
        NERR = 3801
        Return
       Endif
*
       Do  K = 1,NAVPTS
        DBGRD = DBGRD + DMY(K)
       EndDo
       DBGRD = DBGRD/DFLOAT(NAVPTS)
       If(XP(3)(1:1).ne.' ') Then
        Read(XP(3),*,ERR=999) DFUDGE
       Else
        DFUDGE = 0.D0
       Endif
       LX = 4
      ELSE
       READ(XP(1),*,ERR=999) DBGRD
       If(XP(2)(1:1).ne.' ') Then
        Read(XP(2),*,ERR=999) DFUDGE
       Else
        DFUDGE = 0.D0
       Endif
       LX = 3
      ENDIF
*
      IF(DBGRD.LT.0.D0) THEN
       NERR = 3802
       RETURN
      ENDIF
*
*:  subtract background, apply normalization correction
*
      DO  K = 1,KD
       DNC    = (1.D0 + 1.D-4*DFUDGE*DMX(K))
       DMY(K) = DNC*(DMY(K) - DBGRD)
      EndDo
*
*
*:  energy scale corrections
*
      DENSCL = 0.D0
      DENQC  = 0.D0
      If(XP(LX)(1:1).ne.' ')   Read(XP(LX),*,ERR=999) DENSCL
      If(XP(LX+1)(1:1).ne.' ') Read(XP(LX+1),*,ERR=999) DENQC
*
      Do  K = 1,KD
       DMX(K) = (1.D0 + 1.D-3*DENSCL + 1.D-6*DENQC*DMX(K))*DMX(K)
      EndDo
*
*
      Return
*
*:  error exit
*
  999 CONTINUE
      NERR = 3901
*
*
      END
