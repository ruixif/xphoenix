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
      Subroutine CEVAL(XM,ICE,ICD,ICA1,ICA2,ICC1,ICC2,
     +                     ICT1,ICT2,ICT3,ICN,ICM,ICP,ICS,DBGR)
*
* Purpose :  Evaluate input string according to rules that
*            associate meaning to column numbers. The input
*            consists of file name and a set of options.
*            Each input file is assumed to consist of a set of columns.
*            The meaning of the columns has to be defined for each input
*            file.
*            Use the following syntax for a formatted file
*   
*              <file name> E<ne> D<nd> t<nt1> T<nt2> N<nn> B<ff>
*
*            In case of an unformatted input file (like APS binary) use
*
*              <file name> #<ks> E<ne> D<nd> t<nt1> T<nt2> N<nn> B<ff>
*
*            The scan number has to be specified for unformatted files.
*            All variables starting with 'n' have to refer to an existing
*            column number. The meaning of each column is defined by the
*            character that preceeds the number, the specifier. Use the
*            following table of symbols (note case sensivity)
*
*              E,e  =  energy
*              D,d  =  counts
*              a    =  angle position of first crystal
*              A    =  angle position of second crystal
*              c    =  calibration reference for first crystal pos.
*              C    =  calibration reference for second crystal pos.
*              t    =  temperature of first crystal
*              T    =  temperature of second crystal
*              M,m  =  use this data as monitor
*              N,n  =  normalize counts to this data
*              P,p  =  counts of elastic peak spectrum
*              S,s  =  temperature of analyzer crystal
*              B,b  =  constant background for counts
*
*            The following rules apply
*
*              (1)  Unknown specifiers will be ignored, e.g., 'X5'
*                    defaults to '5'.
*              (2)  Numbers without defined specifier will be interpreted
*                    sequentially according to the table of symbols, e.g.,
*                     '2 X7 T9 4' defaults to 'E2 D7 T9 A4'.
*              (3)  The specifiers 't,T,n,N,m,M,s,S,p,P,b,B' are optional.
*              (4)  The specifiers 'E,e' override the specifiers 'a,A', e.g.,
*                    '2 X7 T9 4' is equivalent to 'E2 D7 T9'.
*              (5)  The order of numbers with defined specifiers is
*                    arbitrary, e.g., 'E2 D7 T9' is equivalent to 'D7 T9 E2'
*
* Date    :  March 5, 1997
* Author  :  Wolfgang Sturhahn
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
*
*
      Character*(*) XM
      Character*72  XP(PSPEC),XSPEC
*
*:  separate file name
*
      Call XPARSE(XM,' ',PSPEC,XP)
      XM = XP(1)
*
*:  initialize
*
      ICS  = 0
      ICE  = 0
      ICD  = 0
      ICA1 = 0
      ICA2 = 0
      ICC1 = 0
      ICC2 = 0
      ICT1 = 0
      ICT2 = 0
      ICT3 = 0
      ICM  = 0
      ICN  = 0
      ICP  = 0
*
*:  exceptions
*
      If(XP(2)(1:1).eq.' ' .or. XP(3)(1:1).eq.' ') Return
*
*:  identify columns with undefined specifiers
*
      CALL XFILL(XSPEC,1,72,'B')
      XSPEC(1:12) = 'EDaAcCtTMNPS'
*
      JUC = 0
      Do  J = 2,PSPEC
       If(INDEX('eEdDaAcCtTmMnNpPbBsS#',XP(J)(1:1)).NE.0) Cycle
       JUC   = JUC + 1
       If(INDEX('0123456789',XP(J)(1:1)).eq.0) Then
        XP(J) = XSPEC(JUC:JUC)//XP(J)(2:72)
       Else
        XP(J) = XSPEC(JUC:JUC)//XP(J)(1:71)
       Endif
      EndDo
*
*:  associate column numbers with specifiers
*
      Do  J = 2,PSPEC
       If(XP(J)(1:1).eq.'e' .or. XP(J)(1:1).eq.'E') Then
        Read(XP(J)(2:72),*,ERR=101,END=101) ICE
        ICE = Max0(0,ICE)
        Exit
       Endif
      EndDo
  101 Continue
*
      Do  J = 2,PSPEC
       If(XP(J)(1:1).eq.'d' .or. XP(J)(1:1).eq.'D') Then
        Read(XP(J)(2:72),*,ERR=102,END=102) ICD
        ICD = Max0(0,ICD)
        Exit
       Endif
      EndDo
  102 Continue
*
      Do  J = 2,PSPEC
       If(XP(J)(1:1).eq.'a') Then
        Read(XP(J)(2:72),*,ERR=103,END=103) ICA1
        ICA1 = Max0(0,ICA1)
        Exit
       Endif
      EndDo
  103 Continue
*
      Do  J = 2,PSPEC
       If(XP(J)(1:1).eq.'A') Then
        Read(XP(J)(2:72),*,ERR=104,END=104) ICA2
        ICA2 = Max0(0,ICA2)
        Exit
       Endif
      EndDo
  104 Continue
*
      Do  J = 2,PSPEC
       If(XP(J)(1:1).eq.'t') Then
        Read(XP(J)(2:72),*,ERR=105,END=105) ICT1
        ICT1 = Max0(0,ICT1)
        Exit
       Endif
      EndDo
  105 Continue
*
      Do  J = 2,PSPEC
       If(XP(J)(1:1).eq.'T') Then
        Read(XP(J)(2:72),*,ERR=106,END=106) ICT2
        ICT2 = Max0(0,ICT2)
        Exit
       Endif
      EndDo
  106 Continue
*
      Do  J = 2,PSPEC
       If(XP(J)(1:1).eq.'n' .or. XP(J)(1:1).eq.'N') Then
        Read(XP(J)(2:72),*,ERR=107,END=107) ICN
        ICN = Max0(0,ICN)
        Exit
       Endif
      EndDo
  107 Continue
*
      DBGR = 0.D0
      Do  J = 2,PSPEC
       If(XP(J)(1:1).eq.'b' .or. XP(J)(1:1).eq.'B') Then
        Read(XP(J)(2:72),*,ERR=108,END=108) DBGR
        Exit
       Endif
      EndDo
  108 Continue
*
      Do  J = 2,PSPEC
       If(XP(J)(1:1).eq.'#') Then
        Read(XP(J)(2:72),*,ERR=109,END=109) ICS
        ICS = Max0(0,ICS)
        Exit
       Endif
      EndDo
  109 Continue
*
      Do  J = 2,PSPEC
       If(XP(J)(1:1).eq.'c') Then
        Read(XP(J)(2:72),*,ERR=110,END=110) ICC1
        ICC1 = Max0(0,ICC1)
        Exit
       Endif
      EndDo
  110 Continue
*
      Do  J = 2,PSPEC
       If(XP(J)(1:1).eq.'C') Then
        Read(XP(J)(2:72),*,ERR=111,END=111) ICC2
        ICC2 = Max0(0,ICC2)
        Exit
       Endif
      EndDo
  111 Continue
*
      Do  J = 2,PSPEC
       If(XP(J)(1:1).eq.'p' .or. XP(J)(1:1).eq.'P') Then
        Read(XP(J)(2:72),*,ERR=112,END=112) ICP
        ICP = Max0(0,ICP)
        Exit
       Endif
      EndDo
  112 Continue
*
      Do  J = 2,PSPEC
       If(XP(J)(1:1).eq.'s' .or. XP(J)(1:1).eq.'S') Then
        Read(XP(J)(2:72),*,ERR=113,END=113) ICT3
        ICT3 = Max0(0,ICT3)
        Exit
       Endif
      EndDo
  113 Continue
*
      Do  J = 2,PSPEC
       If(XP(J)(1:1).eq.'m' .or. XP(J)(1:1).eq.'M') Then
        Read(XP(J)(2:72),*,ERR=114,END=114) ICM
        ICM = Max0(0,ICM)
        Exit
       Endif
      EndDo
  114 Continue
*
*:  enforce rules
*
      If(ICE.NE.0) Then
       ICA1 = 0
       ICA2 = 0
      Endif
*
      If(ICA1.eq.0) ICC1 = 0
      If(ICA2.eq.0) ICC2 = 0
*
*
      END
