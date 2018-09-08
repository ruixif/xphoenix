*
**************************************************************
*
*  CONUSS Version 2.1.4
*  Copyright (C) 2016 Wolfgang Sturhahn
*
*  This file is part of CONUSS-2.1.4.
*
*  CONUSS-2.1.4 is free software:
*  You can redistribute it and/or modify it under the terms
*  of the GNU General Public License as published by the
*  Free Software Foundation.
*
*  CONUSS-2.1.4 is distributed in the hope that it will be
*  useful, but WITHOUT ANY WARRANTY; without even the implied
*  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*  PURPOSE. See the GNU General Public License for details.
*
*  A copy of the GNU General Public License should have been
*  provided along with CONUSS-2.1.4.
*  If not, see <http://www.gnu.org/licenses/>.
*
**************************************************************
*
      Function D_RAND(NSEED)
*
* Purpose :  produce random number in [-1,1]
* Date    :  October 28, 2015
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    NSEED    = seed
*
* import variables :
*    D_RAND = random number in [-1,1]
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      Parameter(PG = 48271, PM = 2147483647, DM = 2147483647.0D0)
*
      Data BFIRST /.true./
*
      Save NP
*
*:  deal with seeding the sequence
*
      If(BFIRST) Then
       BFIRST = .false.
       If(NSEED.eq.0) Then
        NP = 0
        Open(UNIT=99,FILE='/dev/random',STATUS='old',
     +      FORM='unformatted',ACCESS='sequential',ERR=100)
        Read(99,ERR=100) NP
        Close(99)
  100   Continue
        If(NP.eq.0)  NP = 1
       Else
        NP = NSEED
       Endif
      Else
*
*:  new number
*
       NP = Mod(NP*PG,PM)
      Endif
*
*:  constrain to [-1,1]
*
      D_RAND = Dfloat(NP)/DM
*
*
      End
