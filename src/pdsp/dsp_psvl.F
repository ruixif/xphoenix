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
      Subroutine DSP_PSVL(NCALL,BRUSE,RSX,RSY,RSDY,RTY,KD,KT1,KT2,KT3,
     +                     PD,ISX,ISY,IWPX,IWPY,XTITLE,XDIR)
*
* Purpose :  display psvl results using g2 graphics
* Date    :  August 27, 2009
* Person  :  Wolfgang Sturhahn
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
* 
#include<implici.h>
*
      Parameter(PIM = 100, PFIT = 100)
*
      Character*(*)  XTITLE,XDIR
      Character*100  XLABEL,XFORM
*
      Dimension  RSX(*),RSY(*),RSDY(*),RTY(*),RSTY(PD,PFIT),RH(PD),
     +           RF1(4),RF2(4),RLOGO(PIM**2),KDT1(PFIT),KDT2(PFIT)
*
      Real*4   DSP_COLOR
      External DSP_COLOR,LENX
*
      Data ISEQ/0/
*
      Save RBLACK,RWHITE,RRED,RGREEN,RBLUE,RYELLO,RGRAY,RGRAY1,RX11,
     +     RXSIZE,RYSIZE,RXOFF,RYOFF,RYMART,RYMARB,RXMARL,RXMARR,
     +     RF1,RF2,RLX,RLY,RLOGO,KDT1,KDT2,RXMIN,RXMAX,RYMIN,RYMAX
*
*
      If(NCALL.lt.1)  Return
*
      BNEW = .not.BRUSE .or. (BRUSE .and. ISEQ.eq.0)
      If(NCALL.eq.1 .and. BNEW) Then
*
*:  all size and position values in pixels
*
*:  window size
*
       RXSIZE = Float(ISX)
       RYSIZE = Float(ISY)
*
*:  window staggering offsets
*
       RXOFF  = -0.05*RXSIZE
       RYOFF  =  0.05*RYSIZE
*
*:  intra-window dimensions
*
       RYMART = 10.
       RYMARB = 40.
*
       RXMARL = 10.
       RXMARR = 10.
*
       RYSEP = 15.
*
*:  X11 window content dimension
*
       RF1(1) = RXMARL + 40.
       RF1(2) = RYMARB
       RF1(3) = RXSIZE - RXMARR - 40.
       RF1(4) = 0.45*RYSIZE - RYSEP
*
       RF2(1) = RXMARL
       RF2(2) = RYMARB + RYSEP + RF1(4)
       RF2(3) = RXSIZE - RXMARR
       RF2(4) = RYSIZE - RYMART
*
*:  X11 window position
*
       RXPOS  = Float(IWPX)
       RYPOS  = Float(IWPY)
       II     = Mod(ISEQ,3)
       RXPOSI = Max1(0.,RXPOS + Float(II)*RXOFF)
       RYPOSI = Max1(0.,RYPOS + Float(II)*RYOFF)
       ISEQ   = ISEQ + 1
       If(BRUSE) Then
        XLABEL = XTITLE
       Else
        IFIG   = Int(Alog10(Float(ISEQ))) + 1
        Write(XFORM,1999) IFIG
        Write(XLABEL,XFORM) XTITLE(1:LENX(XTITLE,' ')),ISEQ
       Endif
*
 1999  Format('(A,''-'',I',I1,')')
*
*:  open X11 device
*
       L = LENX(XLABEL,' ')
       RX11 = G2_OPEN_X11X(RXSIZE,RYSIZE,RXPOSI,RYPOSI,
     +                     XLABEL(1:L),XLABEL(1:L),' ',48.,48.)
*
*:  turn off autoflush to improve performance
*
       Call G2_SET_AUTO_FLUSH(RX11,0.0)
*
*:  allocate basic colors
*
       RBLACK = G2_INK(RX11,0.0,0.0,0.0)
       RWHITE = G2_INK(RX11,1.0,1.0,1.0)
       RRED   = G2_INK(RX11,1.0,0.0,0.0)
       RGREEN = G2_INK(RX11,0.0,1.0,0.0)
       RBLUE  = G2_INK(RX11,0.0,0.6,1.0)
       RYELLO = G2_INK(RX11,1.0,1.0,0.0)
       RGRAY  = G2_INK(RX11,0.8,0.8,0.8)
       RGRAY1 = G2_INK(RX11,0.5,0.5,0.5)
*
*: set background color
*
       Call G2_SET_BACKGROUND(RX11,RBLACK)
*
*:  create color array for logo
*
       Call DSP_LOGO(RX11,PIM,XDIR,NLX,NLY,RLOGO)
       RLX = FLoat(NLX)
       RLY = FLoat(NLY)
      Endif
*
      If(NCALL.eq.1) Then
*
*:  clear previously displayed data
*
       Call G2_CLEAR(RX11)
*
*:  create logo
*
       RX = 0.0
       RY = RYSIZE - RLY
       Call G2_IMAGE(RX11,RX,RY,RLX,RLY,RLOGO)
*
*:  create description
*
       RX = RF2(3) - 80.0
       RY = RF2(4) - 18.0
       XLABEL = 'data & fits'
       Call DSP_TEXT(RX11,RGRAY,18.0,0,RX,RY,XLABEL)
*
*:  find minimum/maximum y-values
*
       RXMIN = RSX(1)
       RXMAX = RSX(KD)
       RXR   = RXMAX - RXMIN
       RXMIN = RXMIN - 0.01*RXR
       RXMAX = RXMAX + 0.01*RXR
*
       RYMIN = RSY(KT1) - 0.5*RSDY(KT1)
       RYMAX = RSY(KT1) + 0.5*RSDY(KT1)
       Do  K = KT1+1,KD
        RYMIN = Amin1(RYMIN,RSY(K)-0.5*RSDY(K))
        RYMAX = Amax1(RYMAX,RSY(K)+0.5*RSDY(K))
       EndDo
       RYR   = RYMAX - RYMIN
       RYMIN = RYMIN - 0.01*RYR
       RYMAX = RYMAX + 0.03*RYR
*
       Do  K = 1,KD
        RH(K) = 0.0
       EndDo
*
*:  plot the data first to avoid painting the frame
*:  plot data as symbols with error bars
*
       Call DSP_PLOT_ERROR_BARS(RX11,RGRAY1,1.0,0,RF2,
     +               RSX,RSY,RH,RSDY,KD,RXMIN,RXMAX,RYMIN,RYMAX)
*
       Call DSP_PLOT(RX11,RGREEN,1.0,4,RF2,RSX,RSY,KD,PD,
     +                  RXMIN,RXMAX,RYMIN,RYMAX)
*
       GoTo 100
      Endif
*
*******************************************************
*
*
      If(NCALL.gt.1 .and. NCALL.le.PFIT+1) Then
       KDT1(NCALL-1) = KT1
       KDT2(NCALL-1) = KT2
*
*:  save data
*
       Do  K = 1,KDT2(NCALL-1)
        RSTY(K,NCALL-1) = RTY(K)
       EndDo
      Endif
*
*******************************************************
*
*
      If(NCALL.eq.KT3+1) Then
*
*:  plot the data first to avoid painting the frame
*
*:  plot data as line
*
       JM  = Min0(PFIT,NCALL-1)
       RDT = 2700./Float(Max0(1,JM-1))
       Do  J = 1,JM
        RTEMP  = 300. + RDT*Float(J-1)
        RCOLOR = DSP_COLOR(RX11,300.,3000.,RTEMP)
*
        Do  K = 1,KDT1(J)
         RTY(K) = RSTY(K,J)
        EndDo
        KKM = KDT1(J)
        Call DSP_PLOT(RX11,RCOLOR,1.0,2,RF2,RSX,RTY,KKM,PD,
     +                  RXMIN,RXMAX,RYMIN,RYMAX)
*
        Do  K = KDT1(J)+1,KDT2(J)-2
         KK      = K - KDT1(J)
         RH(KK)  = RSX(K)
         RTY(KK) = RSTY(K,J)
        EndDo
        KKM = KDT2(J) - KDT1(J) - 2
        Call DSP_PLOT(RX11,RCOLOR,1.0,0,RF2,RH,RTY,KKM,PD,
     +                  RXMIN,RXMAX,RYMIN,RYMAX)
       EndDo
*
*:  create x-axis
*
       Call DSP_XAXIS(RX11,RWHITE,1.0,14.0,RF2,RXMIN,RXMAX,8,
     +                 1,1,1,0,'energy (meV)')
*
*:  create frame
*
       Call DSP_FRAME(RX11,RWHITE,1.0,16.0,RF2,' ')
*
*******************************************************
*
*:  bottom frame
*
*:  create description
*
       RX = RF1(1) + 25.0
       RY = RF1(4) - 19.0
       XLABEL = 'sound velocity'
       Call DSP_TEXT(RX11,RGRAY,14.0,0,RX,RY,XLABEL)
*
       RXX = RX - 10.0
       RYY = RY + 5.0
       Call G2_FILLED_CIRCLE(RX11,RXX,RYY,3.0)
*
       RY = RY - 15.0
       XLABEL = 'chi^2'
       Call DSP_TEXT(RX11,RGRAY,14.0,0,RX,RY,XLABEL)
*
       RYY  = RYY - 15.0
       RXX1 = RXX - 3.0
       RYY1 = RYY - 3.0
       RXX2 = RXX + 3.0
       RYY2 = RYY + 3.0
       Call G2_FILLED_RECTANGLE(RX11,RXX1,RYY1,RXX2,RYY2)
*
*******************************************************
*
*:  plot the data first to avoid painting the frames
*
       JM = Min0(PFIT,NCALL-1)
       Do  J = 1,JM
        RSX(J)  = Float(J)
        RSY(J)  = RSTY(1,J)
        RSDY(J) = RSTY(KDT2(J)-1,J)
        RTY(J)  = RSTY(KDT2(J),J)
       EndDo
       RXMIN = RSX(1)  - 0.5
       RXMAX = RSX(JM) + 0.5
*
       RYMIN = RSY(1)
       RYMAX = RSY(1)
       Do  J = 2,JM
        RYMIN = Amin1(RYMIN,RSY(J))
        RYMAX = Amax1(RYMAX,RSY(J))
       EndDo
       RYR   = RYMAX - RYMIN
       RYMIN = RYMIN - 0.1*RYR
       RYMAX = RYMAX + 0.1*RYR
*
       RYMIN1 = RTY(1)
       RYMAX1 = RTY(1)
       Do  J = 2,JM
        RYMIN1 = Amin1(RYMIN1,RTY(J))
        RYMAX1 = Amax1(RYMAX1,RTY(J))
       EndDo
       RYR   = RYMAX1 - RYMIN1
       RYMIN1 = RYMIN1 - 0.1*RYR
       RYMAX1 = RYMAX1 + 0.1*RYR
*
       RDT = 2700./Float(Max0(1,JM-1))
       Do  J = 1,JM
        RTEMP  = 300. + RDT*Float(J-1)
        RCOLOR = DSP_COLOR(RX11,300.,3000.,RTEMP)
*
        RX  = RSX(J)
        RY  = RSY(J)
        RY1 = RTY(J)
        RDY = RSDY(J)
        Call DSP_PLOT_ERROR_BARS(RX11,RGRAY1,1.0,0,RF1,
     +               RX,RY,0.0,RDY,1,RXMIN,RXMAX,RYMIN,RYMAX)
        Call DSP_PLOT(RX11,RCOLOR,3.0,4,RF1,RX,RY,1,PD,
     +                  RXMIN,RXMAX,RYMIN,RYMAX)
        Call DSP_PLOT(RX11,RCOLOR,3.0,5,RF1,RX,RY1,1,PD,
     +                  RXMIN,RXMAX,RYMIN1,RYMAX1)
       EndDo
*
*******************************************************
*
*:  create x-axis
*
       Call DSP_XAXIS(RX11,RWHITE,1.0,14.0,RF1,RXMIN,RXMAX,8,
     +                 1,1,1,0,'fit sequence')
*
*:  create y-axes
*
       Call DSP_YAXIS(RX11,RWHITE,1.0,14.0,RF1,RYMIN,RYMAX,5,
     +                 1,1,1,0,'sound velocity')
       Call DSP_YAXIS(RX11,RWHITE,1.0,14.0,RF1,RYMIN1,RYMAX1,5,
     +                 2,2,2,0,'chi^2')
*
*:  create frame
*
       Call DSP_FRAME(RX11,RWHITE,1.0,16.0,RF1,' ')
*
*******************************************************
*
      Endif
*
*:  display
*
  100 Continue
      Call G2_FLUSH(RX11)
*
*
      End
