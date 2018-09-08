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
*  Free Software Foundation.
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
      Subroutine READPPM(XFILE,PIM,NX,NY,IMAGE,NERR)
*
* Purpose :  read ppm file
* Date    :  February 25, 2014
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    PIM         = max. nr. of x,y pixels of image
*    XFILE       = file name
*
* import variables :
*    NERR         = error code
*    NX           = nr. of pixels, width
*    NY           = nr. of pixels, height
*    IMAGE(1,*,*) = red value for pixels
*    IMAGE(2,*,*) = green value for pixels
*    IMAGE(3,*,*) = blue value for pixels
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
*
      Character*(*) XFILE
      Dimension  IMAGE(3,PIM,PIM)
*
      Character*1  XR,XG,XB
      Character*72 XLINE
*
*
*:  logical unit used
*                                                                       
      Parameter(LUN = 1)
*
      NERR = 0
*
*:  open input data file
*
      Open(UNIT=LUN, ACCESS='STREAM', FORM='FORMATTED',
     +     STATUS='OLD', FILE=XFILE, ERR=100)
*
    1 Continue
       Read(LUN,'(A)',END=101) XLINE
       If(XLINE(1:1).eq.'#') GoTo 1
*
      If(XLINE(1:2).ne.'P6') Then
       NERR = 1
       Return
      Endif
*
    2 Continue
       Read(LUN,'(A)',END=101) XLINE
       If(XLINE(1:1).eq.'#') GoTo 2
*
      Read(XLINE,*) NX,NY
      If(NX.gt.PIM .or. NY.gt.PIM) Then
       NERR = 2
       Return
      Endif
*
    3 Continue
       Read(LUN,'(A)',END=101) XLINE
       If(XLINE(1:1).eq.'#') GoTo 3
*
      Read(XLINE,*) NC
      If(NC.ne.255) Then
       NERR = 3
       Return
      Endif
*
      Inquire(LUN,POS=NOFF)
      Close(LUN)
*
*:  re-open input data file at previous position
*
      Open(UNIT=LUN, ACCESS='STREAM', STATUS='OLD', FILE=XFILE)
      Read(LUN, POS=NOFF-1) XR
*
      Do  IY = NY,1,-1
       Do  IX = 1,NX
        Read(LUN,END=101) XR,XG,XB
        IMAGE(1,IX,IY) = Ichar(XR)
        IMAGE(2,IX,IY) = Ichar(XG)
        IMAGE(3,IX,IY) = Ichar(XB)
       EndDo
      EndDo
      Close(LUN)
*
      Return
*
  100 Continue
      NERR = 4
      Return
*
  101 Continue
      NERR = 5
*
*
      End
