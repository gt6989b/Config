(defun delete-last-char() 
  (interactive)
  (goto-char (point-max))
  (search-backward "\\")
  (delete-char 1)
  (save-buffer)
)

(define-key global-map "\C-x\C-j" 'delete-last-char)
; to run in batch mode, try
; emacs -batch -l delete.el testfile -f delete-last

(defun goto-percent (pct)
  "for test purpose only"
  (interactive "nPercent: ")
  (goto-char (\ (* pct (point-max)) 100)))

(defun see-bhars ()
  "Displays characters typed, terminated by a 3-second timeout."
  (interactive)
  (let ((chars "")
	(inhibit-quit t))
    (message "Enter characters, terminated by 3-second timeout.")
    (while (not (sit-for 3))
      (setq chars (concat chars (list (read-char)))
	    quit-flag nil))         ; quit-flag maybe set by C-g
    (message "Characters entered: %s" (key-description chars))))


                                                                 
(defun test-fun ()
  (interative)
  (message "for test onoly"))
  
(defun insert-char() 
  (interactive)
  (goto-char (point-max))
  (search-backward "\c****************************************************************************
c The purpose of this routine is to fetch a par curve.
c If bynum = .false., by curve_id
c       CMT  - parcurve based on I25
c       Fxxx - corp FMC
c       Mxxx - muni FMC
c       Ixxx - IYC
c       Cxxx - IYCC
c If bynum = .true., by curve_num
c       dept = muni - Mxxx
c       otherwise   - Fxxx
c     where xxx is a number up to three digits
c Notes: crncy is a dummy and will be removed. You have to know either
c        curve_id or curve_num to call this routine
c****************************************************************************
      subroutine fetchcrv2(curve, dtms, curve_pts, curve_title,
     +     curve_id, curve_dt, volat, max_pts, dept, crncy, curve_num,
     +     curve_freq, bynum, rcode)
c Outputs
      real*8    curve(*)        ! output
      integer*4 dtms(*)         ! days to maturities, will be removed soon
      integer*2 curve_pts       ! # pts in the curve
      character curve_title*(*) ! curve title
      real*8    volat           ! volatility
      integer*4 rcode           ! output error code
c Inputs
      character curve_id*6      ! curve identifier
      integer*2 curve_dt(3)     ! date (can get historical crvs) default=today
      integer*2 max_pts         ! size of array to be filled
      integer*2 dept            ! optional input
      integer*2 crncy           ! optional input - currency
                                   ! fetchcrv will get the govt curve 
                                   ! associated with the currency
      integer*4 curve_num       ! input - fmc curve number (usually)
      integer*2 curve_freq      ! curve freq
      logical   bynum           ! input if true find the curve by number

c Locals
      INTEGER*2 SETCNV
      CHARACTER BIDASK*2


      CHARACTER*80 RCSId
      RCSId=
     +"$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     +"

      BIDASK = 'A'
      SETCNV = 2    ! STANDARD SETTLEMENT  + USE BGN PRICE

      CALL fetchcrv3(curve, dtms, curve_pts, curve_title,
     +     curve_id, curve_dt, volat, max_pts, dept, crncy, curve_num,
     +     curve_freq, bynum,setcnv,bidask,rcode)
      RETURN
      END

c****************************************************************************
      
      subroutine fetchcrv3(curve, dtms, curve_pts, curve_title,
     +     curve_id, curve_dt, volat, max_pts, dept, crncy, curve_num,
     +     curve_freq, bynum,setcnv,bidask,rcode)

      include '/bbinc/d1/g/fetchcrv2_constants.inc'
c outputs
      real*8    curve(*)        ! output
      integer*4 dtms(*)         ! days to mty or mty dts? -will be removed soon
      integer*2 curve_pts       ! # pts in the curve
      character curve_title*(*) ! curve title
      real*8    volat           ! volatility
      integer*4 rcode           ! output error code
c inputs
      character curve_id*6      ! curve identifier
      integer*2 curve_dt(3)     ! date (can get historical crvs) default=today
      integer*2 max_pts         ! size of array to be filled
      integer*2 dept            ! optional input
      integer*2 crncy           ! not used and will be removed
      integer*4 curve_num       ! input - fmc curve number (usually)
      integer*2 curve_freq      ! curve freq
      logical   bynum           ! input if true find curve by curve_num
                                ! else use curve_id
      INTEGER*2 SETCNV          ! if 2: use BGN
      CHARACTER BIDASK*2        ! use bid or ask price for IYC curves

c local
      integer*4    pcs_method    
      integer*4    pcs_number   
      character*8  iyccusip(IYC_CRV_PTS)
      integer*2    iycflg(IYC_CRV_PTS)
      integer*2    iycsflg(IYC_CRV_PTS)
      real*8       ytms_dum(IYC_CRV_PTS)

      CHARACTER*80 RCSId
      RCSId=
     +"$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     +"

      pcs_number = 0

C     translate the meaning of setcnv
      call setcnv_2_pcs_method(setcnv, pcs_method)

      call init_iyc_arrays(iyccusip, iycflg, iycsflg, ytms_dum,
     >     IYC_CRV_PTS)

      call fetchcrv3_agency(
     >     curve,        dtms,       curve_pts,    curve_title,  
     >     curve_id,     curve_dt,   volat,        max_pts,      
     >     dept,         crncy,      curve_num,    curve_freq,
     >     bynum,        bidask,     pcs_method,   pcs_number,
     >     iyccusip,     iycflg,     iycsflg,      IYC_CRV_PTS,
     >     rcode)

      return
      end

c****************************************************************************
c     The difference betweeen fetchcrv3 and fetchcrv3_agency is that the
C     later can specify the curve pricing source and for iyc curves,
C     provide more information  about the curve, including cusip, flag
C     and subflag. 

      subroutine fetchcrv3_agency(
     >     curve,         dtms,        curve_pts,    curve_title,
     >     curve_id,      curve_dt,    volat,        max_pts,
     >     dept,          crncy,       curve_num,    curve_freq,        
     >     bynum,         bidask,      pcs_method,   pcs_number,
     >     cusip_arr,     flg_arr,     sflg_arr,     arr_size,
     >     rcode)

      include '/bbinc/d1/g/seccm.inc'
      include '/bbinc/c1/g/products.inc' 
      include '/bbinc/c5/g/corsctdb.inc' 
      include '/bbinc/c5/g/munsctdb.inc' 
      include '/bbinc/l1/g/oasyc.inc' 
      include '/bbinc/c1/g/acccorp.inc'
      include '/bbinc/l1/g/oascurv.inc'
      include '/bbinc/l1/g/parmcm.inc'
      include '/bbinc/d1/g/swspotcm.inc'
      include '/bbinc/d1/g/yldcomn.inc'
      include '/bbinc/l1/g/mfmchdbcm.inc'
      include '/bbinc/d1/g/oas_statistics.inc'
      include '/bbinc/d1/g/fetchcrv2_constants.inc'

c outputs
      real*8        curve(*)        ! output
      integer*4     dtms(*)         ! days to mty or mty dts? 
                                    ! -will be removed soon
      integer*2     curve_pts       ! # pts in the curve
      character     curve_title*(*) ! curve title
      real*8        volat           ! volatility
      integer*2     curve_freq      ! curve freq
      character*9   cusip_arr(*)    ! cusip arrays, only valid for iyc curves
      integer*2     flg_arr(*)      ! flags, invalid if no cusip
      integer*2     sflg_arr(*)     ! subflags, invalid if no cusip
      integer*4     rcode           ! output error code

c inputs
      character curve_id*6      ! curve identifier
      integer*2 curve_dt(3)     ! date (can get historical crvs) default=today
      integer*2 max_pts         ! size of array to be filled
      integer*2 dept            ! optional input
      integer*2 crncy           ! not used and will be removed
      integer*4 curve_num       ! input - fmc curve number (usually)
      logical   bynum           ! input if true find curve by curve_num
                                ! else use curve_id
      INTEGER*2 SETCNV          ! if 2: use BGN
      CHARACTER BIDASK*2        ! use bid or ask price for IYC curves
      integer*4 pcs_method      ! how to handle pricing source for iyc curves
      integer*4 pcs_number      ! pricing source number when pcs_method 
                                ! indicates it is needed. 
      integer*4 arr_size        ! the size of the cusip, flg, sflg arrays

c functions
      integer*2 int2, fir_get_curve_freq
      integer*2 xpnddt
      logical   fglblchk        ! figgl bit
      logical   cvrel           ! is yldcrv ready? (bfv)
      logical   dtck            ! date check expanded
      logical   dtck2           ! date check unexpanded
      logical   putl_iyc_curve_switch
      logical   fir_init_curve_freq
      logical   putl_get_swap_curve
      integer*4 numdat, ctoi
      integer*4 fmchfindcrv
      character itoc*5
      real*4    fmchvolat

c local variables
      CHARACTER loc_bidask*2    ! local bid or ask for IYCC curves
      integer*2 i2secnum
      integer*4 prcnum, tscode
      real*8    curve32(32)
      integer*4 ii, wlu, i4code, onofftsycrv
      integer*2 i2_one, use, yldcnv
      integer*2 ercode
      logical   dotrc
      character crvtyp*1, c2curr*2
      character iyccusip*8(15)
      integer*2 iycctyp(15), iycdtyp(15), iycfreq(15), dum_ytms(15)
      integer*2 loc_today(3)
      integer*4 termidx(11)/4, 7, 9, 11, 13, 14, 15, 17, 20, 24, 26/
                  ! mapped to standard maturities
                  ! 3m  6m  1y 2y  3y  4y 5y 7y 10y 20y 30y      
      character crvid*6, tmp_curve*6, caller*20
      real*8    epsilon
      parameter (epsilon = 1e-6)
      real*8    coups(11)
      integer*2 elevendtms(11)
      integer*4 oasf_crvnum, cvtlen, kk
      logical   control_bit
      logical   fixed_pcs

C     RCSId was inserted for Ident:
      CHARACTER*80 RCSId
      DATA RCSId /
     +'$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     + $'/

      data elevendtms/91,183,365,731,1096,1461,1826,2557,
     +     3653,7305,10958/ 
      parameter (i2_one=1)
      parameter (onofftsycrv=157)
      automatic curve32, iyccusip
C----------------------------------------------------------------------------
C Trace code logic.

      dotrc=.false. 
      wlu=8
      if (rcode.eq.4HTRC8) dotrc=.true.
      if(fglblchk(0,81,16,i4code)) then
         dotrc = .true.
         write(wlu,*)'fetchcrv3_fnma: oasycnt=',oasycnt
      end if

      rcode = 0

      if (dotrc) then
         write(wlu,1000) dept,curve_num,curve_freq,bynum
 1000    format(' fetchcrv3_fnma: dept=',i2,
     +      ' curve_num=',i6,' curve_freq=',i2,' bynum=',l4)
      endif

c initializations
      curve_pts = 0
      curve_title = ' '
      call init_iyc_arrays(cusip_arr, flg_arr, sflg_arr, dum_ytms,
     >     arr_size)
      
c Parse curve type & curve number
      if(bynum) then
         if(dept.eq.MUNI) then
            crvtyp = 'M'
         else
            crvtyp = 'F'
         end if                  
      else                      ! use curve_id
         crvid = curve_id
         call crv_decode(dept, crvid, crvtyp, curve_num,rcode)
         if (rcode.ne.0) goto 999
      endif
      
      if(dtck(curve_dt,ercode) .and. dtck2(curve_dt,ercode)) then
         loc_today(1) = today(1)
         loc_today(2) = today(2)
         loc_today(3) = xpnddt(today(3))
         call moveit2(loc_today,curve_dt,3)
      endif
      if(dotrc) then
         write(wlu,*)'fetchcrv3_fnma now try to get curve_num=',
     >        curve_num
         write(wlu,*)' dept=',dept,' bynum=',bynum,' crvtyp=',crvtyp
      end if

c SWAP Curves
      if (crvtyp.eq.'S' .or. crvtyp.eq.'s' .and.
     >     putl_get_swap_curve()) then
         curve_pts = 11
         call fir_get_swap_11pt_curve(curve_num, curve_dt, bidask,
     >        curve,  curve_title, curve_freq,rcode)
         if (rcode .ne. 0) goto 999
      else if (crvtyp.eq.'S' .or. crvtyp.eq.'s') then 
c        Assign outputs
         curve_pts = 11
         call oas_getswyc(curve_num, curve_dt, curve_pts, termidx,
     +        curve_title, coups, curve_freq, bidask, rcode)
         if(rcode.ne.0) then
            type *, 'emsg:fetchcrv3_fnma:oas_getswyc rc=', rcode,
     >           ' curve_num=', curve_num
            rcode = 5551
            goto 999
         endif
         call condition_curve(coups, yc11_ytms, int(curve_pts),
     +        curve, yc11_ytms, int(curve_pts), rcode)
         if(rcode.ne.0) then
            type *,'emsg:fetchcrv3_fnma:condition_curve rc=', rcode,
     >           ' swyc curve_num=', curve_num, ' curve_pts=', curve_pts
            rcode = 5552
            goto 999
         endif
c        Assign common area
         OASYCNT = curve_pts
         OASYCFRQ = curve_freq
         call moveit(yc11_time,OASYCTIM,OASYCNT)
         call moveit(curve,OASYCURV,OASYCNT*2)         

c IYC curves
      else if (crvtyp.eq.'I' .and. putl_iyc_curve_switch()) then
         curve_pts = 11
c     the arr_size represents the size of the arrays cusip_arr, flg_arr,
C     sflg_arr, could be larger than 11. 
         call fir_get_iyc_11pt_curve(
     >        curve_num,    curve_dt,    bidask,      pcs_method,
     >        pcs_number,   cusip_arr,   flg_arr,     sflg_arr,
     >        curve,        arr_size,    curve_title, curve_freq,
     >        rcode)
         if (rcode .ne. 0) goto 999
      else if (crvtyp.eq.'I') then
c     this part will be retired when fir_get_iyc_curve works fine.
C     Therefore, the changes in this part is kept at minimum. 
         c2curr = ' '
         if (curve_num .eq. 18) then   
            yldcnv = 2  ! use the semiannual compounding for japanese curve. 
         else 
            yldcnv = 1  ! default 
         endif
         curve_dt(3) = xpnddt(curve_dt(3))

         control_bit= fglblchk(0,135,5,i4code)
         if (i4code .ne. 0) control_bit = .FALSE.
         tmp_curve = oasf_curve
         oasf_crvnum = ctoi(tmp_curve(2:), cvtlen)

C     fixed_pcs is needed just to ensure we can enable to switch to the
C     old code in case anything wrong occurs in the new code. 
         fixed_pcs = pcs_method .eq. PCS_USE_PRICING_NUMBER
         if (fixed_pcs .and. (curve_num .eq. oasf_crvnum)
     >        .and. control_bit) then 
            prcnum = 1
            tscode = 0
            i2secnum = 1
            setcnv = 1   !use standard settlement without forcing source BGN
            bidask='B'
            rcode=oasf_pricing_source
            call iycinfo4(i2secnum,curve_num,curve_dt, c2curr, 
     >           yldcnv,setcnv,iycctyp,iycdtyp,iycfreq,iyccusip,bidask
     >           ,prcnum,tscode,rcode)
            if (rcode .ne. 0) then
               type *, 'emsg:fetchcrv3_fnma:iycinfo4 rc=', rcode,
     >              ' curve_num=', curve_num, ' c2curr=', c2curr
               rcode = 20010604
               goto 999
            endif
         else 
            if(fixed_pcs .and. curve_num .eq. oasf_crvnum) then 
               type *
     >           ,'emsg:fetchcrv3_fnma:the figgle bit for OASF is off'
               rcode = 20010604
               goto 999
            else 
               call iycinfo2(curve_num,curve_dt,c2curr,yldcnv,setcnv,
     +              iycctyp,iycdtyp,iycfreq,iyccusip,bidask,rcode)
               if (rcode .ne. 0) then 
                  type *, 'emsg:fetchcrv3_fnma:iycinfo2 rc=', rcode,
     >                 ' curve_num=', curve_num, ' c2curr=', c2curr
               endif
            endif
         endif 
         if(rcode.ne.0) then
            rcode = 5553
            goto 999
         endif
c        Assign outputs
         if (fir_init_curve_freq()) then 
            curve_freq=fir_get_curve_freq(curve_num, iycfreq, 15, rcode)
            if (rcode .ne. 0) then 
               rcode=5555
               goto 999
            endif
         else
            do ii = 15, 1, -1
               if(iycfreq(ii).ne.0) then
                  curve_freq = iycfreq(ii)
                  goto 1
               endif
            enddo
1           if(curve_freq.eq.0) then
               type *, 'emsg:fetchcrv3_fnma:curve_freq=0 curve_num=',
     >              curve_num
               rcode = 5555
               goto 999
            endif
         endif
         curve_title = ycrvtitl
         curve_pts = 11
         call condition_curve(ycrvylds, yc15_ytms, 15,
     +        curve, yc11_ytms, int(curve_pts), rcode)
         if(rcode.ne.0) then
            type *, 'emsg:fetchcrv3_fnma:condition_curve rc=', rcode,
     >           'iyc curve_num=', curve_num, ' curve_pts=', curve_pts
            rcode = 5552
            goto 999
         endif
c        Assign common area
         OASYCFRQ = curve_freq
         OASYCNT = curve_pts
         call moveit(yc11_time,oasyctim,OASYCNT)
         call moveit(curve,OASYCURV,OASYCNT*2)

C    Added curves generated by IYCC - customized IYC curves
C    - L. Wang 10/98
      else if(crvtyp.eq.'C'.and. curve_id.ne.'CMT') then
         c2curr = ' '
         yldcnv = 1
         curve_dt(3) = xpnddt(curve_dt(3))
         if (fir_init_curve_freq()) then 
c     change crv-custinfo1 to crv_custinfo2 because the former does not
C     return iycfreq. bidask set to b because this is the way to
C     crv_curstinfo1 was written.     5/31/2002 ZH
            loc_bidask='B'
            call crv_custinfo2(curve_num,curve_dt,c2curr,yldcnv,setcnv,
     +           iycctyp,iycdtyp,iycfreq,iyccusip,loc_bidask,rcode)
         else 
            call crv_custinfo1(curve_num,curve_dt,c2curr,yldcnv,setcnv,
     +           bidask,rcode)
         endif
         if(rcode.ne.0) then
            type *, 'emsg:fetchcrv3_fnma:crv_custinfo1 rc=', rcode,
     >           ' curve_num=', curve_num
            rcode = 5556
            goto 999
         endif
c        Assign outputs
         if (fir_init_curve_freq()) then 
            curve_freq=fir_get_curve_freq(curve_num, iycfreq, 15, rcode)
            if (rcode .ne. 0) then 
               rcode=5555
               goto 999
            endif
         else    
            do ii = 15, 1, -1
               if(iycfreq(ii).ne.0) then
                  curve_freq = iycfreq(ii)
                  goto 2
               endif
            enddo
2           if(curve_freq.eq.0) then
               type *, 'emsg:fetchcrv3_fnma:curve_freq=0  curve_num=', 
     >              curve_num
               rcode = 5555
               goto 999
            endif
         endif
         curve_title = ycrvtitl
         curve_pts = 11
         call condition_curve(ycrvylds, yc15_ytms, 15,
     +        curve, yc11_ytms, int(curve_pts), rcode)
         if(rcode.ne.0) then
            type *, 'emsg:fetchcrv3_fnma:condition_curve rc=', rcode,
     >           ' custom iyc curve_num=', curve_num, ' curve_pts=',
     >           curve_pts
            rcode = 5552
            goto 999
         endif
c        Assign common variables
         OASYCFRQ = curve_freq
         OASYCNT = curve_pts
         call moveit(yc11_time, OASYCTIM, OASYCNT)         
         call moveit(curve, OASYCURV, OASYCNT*2)

c FMC Curves         
      else if(crvtyp.eq.'F') then
         curve_pts = 11
c        Historical
         if(numdat(curve_dt).ne.numdat(today)) then
            call getccrv(curve_num, rcode)
            if(rcode.ne.0 .or. csct_syield(1).eq.0 .or.
     +           .not. cvrel(int(csct_type))) then
               type *,'emsg:fetchcrv3_fnma:getccrv rc=',rcode,'crv#='
     +              ,curve_num,csct_syield(1),cvrel(int(csct_type))
               rcode = 5554
               goto 999
            endif
            i4code = fmchfindcrv(int2(curve_num), numdat(curve_dt))
            if(i4code.ne.0 .and. i4code.ne.1) then
               rcode = i4code
               goto 999
            end if
            call getcfmchylds(curve32)
            call yc_old11(curve32,curve)
            volat = fmchvolat()
         else
c           Today's
            call getccrv(curve_num, rcode)
            if(rcode.ne.0 .or. csct_syield(1).lt.epsilon .or.
     +           .not. cvrel(int(csct_type))) then
               type *,'emsg:fetchcrv3_fnma:getccrv rc=',rcode,' crv#='
     +              ,curve_num, ' csct_syield(1)=', csct_syield(1)
     >              ,' cvrel(int(csct_type))=', cvrel(int(csct_type))
               rcode = 5554
               goto 999
            endif
            call moveit(csct_syield,curve,2*curve_pts)
            volat = csct_volat
         end if
         curve_title = csct_title
         curve_freq = csct_freq         
         do ii = 1, curve_pts
            dtms(ii) = nint(yc11_time(ii) * 365.25)
         enddo
c        assign common area
         OASYCNT = curve_pts
         OASYCFRQ = curve_freq
         call moveit(yc11_time, OASYCTIM, OASYCNT)
         call moveit(curve, OASYCURV, 2*OASYCNT)            

c MUNI FMC curves
      else if(crvtyp.eq.'M') then
c        Historical
         if(numdat(curve_dt).ne.numdat(today)) then
            call getmcrv(curve_num, .false., rcode)
            if(rcode.ne.0 .or. msct_syield(1).lt.epsilon) then
               type *,'emsg:fetchcrv3_fnma:getmcrv rc=',rcode,' crv#='
     +              ,curve_num, ' msct_syield(1)=', msct_syield(1)
               rcode = 5557
               goto 999
            else
               curve_title = msct_title
               curve_freq = 2
            end if
            mfmch_func = 1
            mfmch_ix = 0
            mfmch_kl = 6
            mfmch_crvnum = curve_num
            mfmch_date = numdat(curve_dt)
            call accmfmch
            if(mfmch_ret.eq.0) then
               do ii = 1,32
                  curve32(ii) = float(mfmch_ylds(ii))/1d3
               end do
               volat = mfmch_volat/1d2
               if(max_pts.lt.32) then
                  call yc_old11(curve32,curve)
                  curve_pts = 11
                  do ii = 1,11
                     dtms(ii) = nint(yc11_time(ii) * 365.25)
                  end do
                  oasycnt = 11
                  call moveit(yc11_time,oasyctim,11)
                  call moveit(curve,oasycurv,22)
               else
                  call moveit(curve32,curve,32*2)
                  curve_pts = 32
                  do ii = 1, 32
                     dtms(ii) = nint(yc32_time(ii) * 365.25)
                  end do
                  oasycnt = 32
                  call moveit(yc32_time,oasyctim,32)
                  call moveit(mfmch_ylds,oasycurv,64)
               end if
            else
               rcode = mfmch_ret
            end if
         else
c           Today's
            if (dotrc) rcode=4HTRC8
            call getmcrv(curve_num, .false., rcode)
            if(rcode.ne.0 .or. msct_syield(1).lt.epsilon) then
               if(dotrc) then
                  write(wlu,1005) rcode,msct_syield(1)
1005              format(' fetchcrv3_fnma: aft getmcrv rcode=',i4,
     +                 'msct_syield(1)=',f12.5)
               end if
               type *,'emsg:fetchcrv3_fnma:getmcrv rc=',rcode,' crv#='
     +              ,curve_num, ' msct_syield(1)=', msct_syield(1)
               rcode = 5557      !no yield curve for currency
               goto 999
            endif         
            call moveit(msct_syield(1),curve(1),22)
            do 6 ii = 1, 11
               dtms(ii) = nint(yc11_time(ii) * 365.25)
6           continue
            call moveit(msct_32yld(1),oasycurv(1),64)
            curve_pts = 11
            oasycnt = 32
            call moveit2(yc32_time,oasyctim,64)
            curve_freq = 2
            curve_title = msct_title
         end if

c Constant Maturity US Treasury
      else if(curve_id.eq.'CMT') then
         loc_today(1) = today(1)
         loc_today(2) = today(2)
         loc_today(3) = xpnddt(today(3))
         call moveit2(loc_today,curve_dt,3)
         curve_dt(3) = xpnddt(curve_dt(3))
         USE = 0
         CALL BSET(USE, 7)
         CALL BSET(USE, 9)
         CALL BSET(USE,10)
         CALL BSET(USE,12)
         CURVE_FREQ = 2
         if (dotrc) rcode=4HTRC8
         CALL PARCURVE(25,CURVE_FREQ,USE,RCODE) ! get IYC curve
         if (dotrc .and. rcode.ne.0) then
            write(wlu,1004) rcode
1004       format(' fetchcrv3_fnma: aft parcurve rcode=',i4)
         endif
c        assign outputs 
         CALL MOVEIT(OASYCURV, CURVE, 22)
         CSCT_TITLE = 'Const. Mty Tsy Curve'
         curve_title = 'Const. Mty Tsy Curve'
         curve_pts = 11
         oasycnt = curve_pts
         do 24 ii = 1, curve_pts
            dtms(ii) = nint(oasyctim(ii) * 365.25)
24       continue
      endif

c Overwrite the volatilty with the 3mon rate - To be removed
      if(flg(1) .ne. MTGE) then
         if(bynum) crvid = crvtyp//itoc(curve_num,kk)
c         call fetch_curve_vol(crvid, curve_dt, volat)
      endif

999   continue
      if(rcode.ne.0) then
         caller='fetchcrv3_fnma'
         call oas_print_sec(caller)
         curve_pts = 11
         do ii = 1, curve_pts
            curve(ii) = 1.0d0   ! set to flat 1% to single a wrong curve
         enddo
         OASYCNT = curve_pts
         OASYCFRQ = 2
         call moveit(curve, OASYCURV, oasycnt*2)
         call moveit(yc11_time, OASYCTIM, oasycnt)
      else
         OASYCID = CRVID        ! more common area assignment in the future
      endif

      return
      end

c******************************************************************************
c     The purpose of this program is to load a swap par curve given a curve
c     number and the number of points on the curve. The returned rates should
c     match with SWYC function.
c     - Laura H. Wang 12/99
c     rcode: 1 ~ 3 - bad input
c            4 - accswyc96 bad return
c            5 - swdfl96 bad return
c            6 - tNrates bad return
c******************************************************************************
      subroutine oas_getswyc(curve_num,  curve_dt, curve_pts, termidx,
     +                       curve_title, coups, freq, bidask, rcode)

      include '/bbinc/d1/g/swspotcm.inc' ! Common area for SWYCs
      include '/bbinc/d1/g/indxmodel.inc'
      include '/bbinc/d1/g/parmcm6.inc'

c Inputs
      integer*2 curve_dt(3)     ! curve date
      integer*2 curve_pts       ! number of points on the curve
      integer*4 curve_num       ! curve number
      integer*4 termidx(*)      ! the termidx on SWYC fucntion
                                ! e.g., the termidx for 3m is 4
      character bidask*2        ! bid or ask side rates

c Outputs
      integer*2 freq            ! curve frequency
      integer*4 rcode           ! return code
      real*8    coups(*)        ! rates of the curve
      character curve_title*(*) ! curve title
      
c Locals
      character p_bidask*2, r_bidask*2       
      integer*2 settl(3), dtdum(3), daytyp(2), i
      integer*4 seq_num, ercode, inc, uuid
      real*8    r8dum
      character fxfrq*2, flfrq*2, flrset*2, c2curr*2, cntry*2, isocurr*3
      
c Functions
      integer*4 swycm2p
      character swyc_crvtitle*20, leftjus*80, swyc_ccy2*2, fxbb2iso*3
      character iso2cdr*2
      logical dtckexp, samedayswap


      CHARACTER*80 RCSId
      RCSId=
     +"$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     +"

      rcode = 0
      ercode = 0

c Check data
      if(dtckexp(curve_dt,ercode)) then
         rcode = 1
         goto 999
      endif
      if(curve_pts.le.0) then
         rcode = 2
         goto 999
      endif
      
      do i = 1, curve_pts
         if(termidx(i).le.0) then
            rcode = 3
            goto 999
         endif
      enddo

c Load the common area for SWYCs
      r8dum = 0          
      c2curr = ' '
      seq_num = swycm2p(curve_num)
      uuid = P6UUID
      c2curr = swyc_ccy2(seq_num, uuid)
      isocurr = fxbb2iso(c2curr)
      cntry = iso2cdr(isocurr)   

      if(samedayswap(cntry)) then
         inc = 0
      else
         inc = 2
      endif
      call addbusdy(cntry, curve_dt, inc, settl, ercode) 

      if(ercode.ne.0) then
         rcode = 4
         goto 999
      endif

      p_bidask=bidask
      r_bidask=bidask
      call accswyc96(curve_num, c2curr, curve_num, c2curr,
     +     p_bidask, r_bidask, curve_dt, settl, r8dum, r8dum, ercode)
      call accswyc96_check_bidask_changed(p_bidask, r_bidask, bidask,
     >     curve_num)
      if(ercode.ne.0) then
         rcode = 5
         goto 999
      endif

c Get curve title
      curve_title = 'Swap: '//leftjus(swyc_crvtitle(seq_num))

c Get freqency
      call moveit2(curve_dt,dtdum,3)
      dtdum(3) =  dtdum(3) + 5
      call swdfl96(p6uuid, seq_num, curve_dt, dtdum, daytyp,
     +     fxfrq, flfrq, flrset, ercode)
      if(ercode.ne.0) then
         rcode = 6
         goto 999
      endif

      call numfreq(fxfrq, freq)

c Convert spot curve to par curve
      call tNrates(settl, c2curr, daytyp(2), daytyp(1), freq, curve_pts,
     +     termidx, pspot_s_sw, pdtms_sw, pcrvcnt_sw, coups, ercode)
      if(ercode.ne.0) then
         rcode = 7
         goto 999
      endif

 999  continue
      return
      end

c****************************************************************************
c     Function to pick the rates based on the termidx and crvpts nad convert
c     the rate to par coupon
c     Laura H. Wang, 12/99
c*****************************************************************************
      subroutine tNrates(settl, ccy, dr_daytyp, sr_daytyp, sr_freq,
     +                   crvpts, termidx, spots, dtms, crvcnt, coups, 
     +                   rcode)

c Inputs
      integer*2   settl(3), dr_daytyp, sr_daytyp, sr_freq, dtms(*),
     +            crvcnt, crvpts
      integer*4   termidx(*)
      character*2 ccy
      real*8      spots(*)

c Outputs
      integer*4   rcode
      real*8      coups(*)

c Locals
      integer*2    i, mtydt(3), iccy, cnt
      integer*4    end, begin, ercode
      character*2  cntry

C Functions
      integer*2    cntrlbln, actday, int2
      character*2  bdaycode
      real*8       spt2cp96
      logical      one_yr_swap_rt

C MTP these need to be initialized...05/10/2001

      CHARACTER*80 RCSId
      RCSId=
     +"$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     +"

      do i = 1,crvpts
        coups(i) = 0d0
      enddo

      end = 3
      begin = 4
      if (one_yr_swap_rt(ccy)) then
         end = 2
         begin = 3
      endif

      ercode = 0
      rcode = 0

      iccy = CNTRLBLN(ccy)
      CNTRY = BDAYCODE(iccy,ERCODE)
      if(ercode.ne.0) then
         rcode = 1
         goto 999
      endif

      cnt = 0

      do 10 i = 1, end
         cnt = cnt+1
         call mktermdt(termidx(i),settl,cntry,mtydt,ercode)
         if(ercode.ne.0) then
            rcode = 2
            goto 999
         endif
         coups(cnt) = spt2cp96(crvcnt,spots,dtms,
     +                         settl,int2(0),dr_daytyp,mtydt,ccy)*1d2
 10   continue

      do 20 i = begin, crvpts
         call mktermdt(termidx(i),settl,cntry,mtydt,ercode)
         if(ercode.ne.0) then
            rcode = 3
            goto 999
         endif
         coups(cnt+1) = 0d0
         if (actday(settl,mtydt).gt.dtms(crvcnt)) goto 20
         cnt = cnt+1
         coups(cnt) = spt2cp96(crvcnt,spots,dtms,
     +                         settl,sr_freq,sr_daytyp,mtydt,ccy)*1d2
 20   continue


 999  continue
      return
      end

      SUBROUTINE NUMFREQ(INFRQ, OUTFRQ)

      INTEGER*2   OUTFRQ, FRQI(5), I
      CHARACTER   INFRQ*2, FRQS(5)

  

C     RCSId was inserted for Ident:
      CHARACTER*80 RCSId
      DATA RCSId /
     +'$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     + $'/

      DATA FRQI/   1,   2,   4,  12,  52/
      DATA FRQS/'A ','S ','Q ','M ','W '/


      DO 10 I = 1,5
         IF (INFRQ.EQ.FRQS(I)) GOTO 15
10    CONTINUE
      OUTFRQ = 0
      RETURN
15    CONTINUE

      OUTFRQ = FRQI(I)
      RETURN
      END

c******************************************************************************
c  The purpose of this subroutine is to decode the 6 character crvid
c  to 1 character crvtyp, i.e., 'F','M','I','S','C', and curve_num under
c  these curve types.
c  by Laura Wang - 4/00
c******************************************************************************

      subroutine crv_decode(dept, crvid, crvtyp, curve_num, rcode)

      include '/bbinc/c1/g/products.inc'

c Inputs
      integer*2 dept

c Inputs/Outputs
      character crvid*6
      
c Outputs
      character crvtyp
      integer*4 curve_num
      integer*4 rcode 
      
c Locals
      integer*2 i, ii, l
      character    local_crvid*6

c Functions
      integer*4 slen
      integer*4 ctoi
      logical   is_num
      

      CHARACTER*80 RCSId
      RCSId=
     +"$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     +"

      rcode = 0

      l = slen(crvid)

      if(l.le.0) then
         rcode = 1
         goto 999
      endif

      call toupper(crvid)

c     Get crvtyp and curve_num

c     No letters are allowed other than the first letter or CMT
      if(crvid.eq.'CMT') then
         crvtyp = ' '
         curve_num = 0
         goto 999
      endif

      crvtyp = crvid(1:1)
      if(is_num(crvtyp).or.
     +     crvtyp.eq.'I' .or.   ! IYCs
     +     crvtyp.eq.'F' .or.   ! Corp FMCs
     +     crvtyp.eq.'M' .or.   ! Muni FMCs
     +     crvtyp.eq.'C' .or.   ! Customized IYCs(IYCC) and CMT
     +     crvtyp.eq.'S' ) then ! SWYCs
         do i = 2, l  
            if (.not.is_num(crvid(i:i))) then
               rcode = 1
               goto 999
            endif
         enddo
      else
         rcode = 1
         goto 999
      endif

      if(is_num(crvtyp)) then
         local_crvid = crvid
         curve_num = ctoi(crvid,ii)
         if(dept.eq.MUNI) then
            crvtyp = 'M'
            crvid = 'M' // local_crvid(1:)
         else
            crvtyp = 'F'
            crvid = 'F' // local_crvid(1:)
         end if
      else
         curve_num = ctoi(crvid(2:),ii)   ! ii should changed to i4
         if(curve_num.le.0) then
            rcode = 1
            goto 999
         endif
      endif
      
 999  return
      end
      
      subroutine condition_curve(in_ylds, in_ytms, in_num,
     +     out_ylds, out_ytms, out_num, rcode)

c Inputs
      real*8 in_ylds(*)
      real*8 in_ytms(*)
      integer*4 in_num
      real*8 out_ytms(*)
      integer*4 out_num

c Outputs
      real*8 out_ylds(*)
      integer*4 rcode

c Locals
      integer*2  maxnpts
      parameter  (maxnpts = 50)
      real*8     tmp_ylds(maxnpts)
      real*8     tmp_ytms(maxnpts)
      integer*2  curve_pts
      integer*2  i
      real*8     epsilon
      parameter  (epsilon = 1e-6)



      CHARACTER*80 RCSId
      RCSId=
     +"$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     +"

      rcode = 0

      curve_pts = 0
      do i = 1, in_num
         if(abs(in_ylds(i)) .gt. epsilon) then
            curve_pts = curve_pts + 1
            if (curve_pts .gt. maxnpts) then
               rcode = 1
               return
            endif
            tmp_ytms(curve_pts) = in_ytms(i)
            tmp_ylds(curve_pts) = in_ylds(i)
         endif
      enddo      
      if(curve_pts.eq.0) then
         rcode = 2
         return
      endif

      do i = 1, out_num
         call mtg_intptsy(tmp_ytms, tmp_ylds, int(curve_pts),
     +        out_ytms(i), out_ylds(i))
      enddo

      return
      end


      logical function check_bidask_changed()
      logical fir_check_putl_byte


C     RCSId was inserted for Ident:
      CHARACTER*80 RCSId
      DATA RCSId /
     +'$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     + $'/

      check_bidask_changed=fir_check_putl_byte(608,
     >     'check_bidask_changed', 0)
      end

      subroutine accswyc96_check_bidask_changed(p_bidask, r_bidask,
     >     bidask, curve_num)
      character p_bidask*2, r_bidask*2, bidask*2
      integer*4 curve_num

c     local
      character caller*20
c     function
      logical check_bidask_changed


C     RCSId was inserted for Ident:
      CHARACTER*80 RCSId
      DATA RCSId /
     +'$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     + $'/

      caller='accswyc96'
      if (check_bidask_changed()) then 
         if (p_bidask .ne. bidask) then
            type *,'emsg:accswyc96_check_bidask_changed:',
     >           '<<FIR>>bidask changed from bidask=',bidask,' to
     >           p_bidask=', p_bidask,' curve_num=', curve_num 
            call oas_print_sec(caller)
         endif
         if (r_bidask .ne. bidask) then
            type *,'emsg:accswyc96_check_bidask_changed:',
     >           '<<FIR>>bidask changed from bidask=',bidask,' to
     >           r_bidask=', r_bidask,' curve_num=', curve_num 
            call oas_print_sec(caller)
         endif
      endif

      end

*******************************************************************************
c     This routine obtains the iyc curve and copies it to the oasyc
C     common area.  Besides wrapping iycinfo calls in one routine, the
C     way  iycinfo2 or iycinfo4 gets called is also changed.  Now 
c     loc_bidask=' ' is passed and get back ycrvask or ycrvbid according
C     to the passed in argument bidask.  

c     The switch to use this code has been turned off as of 5/17/2002,
C     because, psg change their mind to show ask or bid for iyc curve curve
c     as described in drqs 1752726.

c     12/31/2002: Enable this routine for the purpose of modulization.
C     1. The else part corresponding to the switch
C     putl_get_iyc_curve should be deleted after certain amount
C     of time code test in production. 
c     2. To be consistent with the old code, the way iycinfo4 or
C     iycinfo2 gets call is changed back to the old style, i.e. pass
C     bidask directly to the iycinfo routine, and obtain the curve from
C     a single array ycrvylds, instead of from ycrvbid or ycrvask.
C     However, to handle the case where bidask = ' ' is needed when
C     calling  iycinfo2, it's now enough to pass bidask=' ' to iycinfo2
C     and change the first bidask to ' ' when calling
C     copy_iyc_curve_from_common.

c*****************************************************************************

      subroutine fir_get_raw_iyc_curve( 
     >     curve_num,     curve_dt,   bidask,    pcs_method,
     >     pcs_number,    iyccusip,   iycflg,    iycsflg,
     >     iyccurve,      iyc_ytms,   arr_size,  curve_title,
     >     curve_freq,    rcode)

      include '/bbinc/d1/g/fetchcrv2_constants.inc'
      include '/bbinc/d1/g/yldcomn.inc'

c     input
      integer*4   curve_num        
      integer*2   curve_dt(3)      
      integer*4   pcs_method
      integer*4   pcs_number
      character   bidask*2 
      integer*4   arr_size

c     output
      character*8 iyccusip(*)
      integer*2   iycflg(*)
      integer*2   iycsflg(*)
      real*8      iyccurve(*)         
      real*8      iyc_ytms(*)         
      character   curve_title*(*)  
      integer*2   curve_freq       
      integer*4   rcode            

c     local
      integer*4   i4code
      integer*4   prcnum, tscode
      integer*2   i2secnum, yldcnv
      integer*2   iycctyp(IYC_CRV_PTS), iycdtyp(IYC_CRV_PTS),
     >     iycfreq(IYC_CRV_PTS)
      character   iyc_bidask*2, c2curr*2
      integer*2   iyc_setcnv

c     functions
      integer*2   xpnddt, fir_get_curve_freq

C     RCS id (do not change)
      CHARACTER*80 RCSid
      DATA RCSid /
     +'$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     +$'/

      rcode = 0

      call init_iyc_arrays(iyccusip, iycflg, iycsflg, iyc_ytms,
     >     arr_size)
         
      if (arr_size .lt. IYC_CRV_PTS) then 
c     Note for non-iyc curves, the number of points in the curve may still
C     be larger than IYC_CRV_PTS. However, for these curves, the iyc
C     arrays only be initialized. 
         rcode = CRV_ERROR_SMALL_SIZE_RAWCRV
         return
      endif

      call set_yield_convention(curve_num, yldcnv)
      
      c2curr = ' '

      curve_dt(3) = xpnddt(curve_dt(3))
      call set_parameters_for_iycinfo4(
     >     pcs_method,    pcs_number,     bidask,      prcnum,
     >     tscode,        i2secnum,       iyc_setcnv,  iyc_bidask,
     >     rcode,         i4code)
      if (i4code .ne. 0) then 
         rcode = i4code + 5500
         return
      endif
      call iycinfo4(
     >     i2secnum,  curve_num,  curve_dt,   c2curr,
     >     yldcnv,    iyc_setcnv, iycctyp,    iycdtyp,
     >     iycfreq,   iyccusip,   iyc_bidask, prcnum,
     >     tscode,    rcode)
      if (rcode .ne. 0) then
         type *,'emsg:fir_get_raw_iyc_curve:<<FIR>>iycinfo4 rc=',
     >        rcode,' curve_num=', curve_num, ' c2curr=', c2curr
         rcode = 5558
         return
      endif
      
      curve_freq=fir_get_curve_freq(curve_num, iycfreq, 15, rcode)
      if (rcode .ne. 0) then 
         rcode=5555
         return
      endif

      curve_title = ycrvtitl
      call copy_iyc_curve_from_common(
     >     iyc_bidask,  bidask,   iycflg,    iycsflg,
     >     iyccurve,    iyc_ytms, arr_size,  rcode)

      return
      end

c**************************************************************************
c     get a subset of iyc curve according to the input ytms array and
C     extend the curve by linear interplate 
c**************************************************************************

      subroutine fir_get_interplated_iyc_curve(
     >     curve_num,     curve_dt,      bidask,    pcs_method,
     >     pcs_number,    in_ytms,       arr_size,  cusip_arr,
     >     flg_arr,       sflg_arr,      curve,     curve_title,
     >     curve_freq,    rcode)

      include '/bbinc/d1/g/fetchcrv2_constants.inc'

c     input
      integer*4   curve_num        
      integer*4   pcs_method 
      integer*4   pcs_number
      integer*2   curve_dt(3)      
      character   bidask*2    
      real*8      in_ytms(*)  ! only yield in these maturities are needed
      integer*4   arr_size    ! the size of all output arrays
     
c     output
      real*8      curve(*)         
      character   curve_title*(*)  
      integer*2   curve_freq       
      character*8 cusip_arr(*)
      integer*2   flg_arr(*)
      integer*2   sflg_arr(*)
      integer*4   rcode            

c     local
      integer*4   wlu
      logical     debug
      real*8      iyccurve(IYC_CRV_PTS)
      real*8      iyc_ytms(IYC_CRV_PTS)
      character*9 iyccusip*8(IYC_CRV_PTS)
      integer*2   iycflg(IYC_CRV_PTS)
      integer*2   iycsflg(IYC_CRV_PTS)
      integer*2   indices(IYC_CRV_PTS), k, i
      real*8      tolerance
      parameter   (tolerance  =  1E-10)

      rcode = 0

      debug = .false.

      call  fir_get_raw_iyc_curve(
     >     curve_num,      curve_dt,      bidask,      pcs_method,
     >     pcs_number,     iyccusip,      iycflg,      iycsflg,
     >     iyccurve,       iyc_ytms,      IYC_CRV_PTS, curve_title,
     >     curve_freq,     rcode)

      if (rcode .ne. 0) return

      call condition_curve(iyccurve, iyc_ytms, IYC_CRV_PTS,
     +     curve,  in_ytms,  arr_size,  rcode)
      
      if (rcode.ne.0) then
         rcode = 5552
         return
      endif

c     find which indices in the original iyc curve correspond to the new
C     maturity array, and then copy the cusip, flg, sflg accordingly. 
      call fir_matching_subset(
     >     iyc_ytms,     IYC_CRV_PTS,     in_ytms,
     >     arr_size,     tolerance,       indices,
     >     rcode)
      
      if (rcode .ne. 0) then 
         rcode = 5561
         return
      endif

      do i = 1, arr_size
         k = indices(i)
         if (k .gt. IYC_CRV_PTS) then 
            rcode = 5562
            return
         endif

         cusip_arr(i) = iyccusip(k)
         flg_arr(i)   = iycflg(k)
         sflg_arr(i)  = iycsflg(k)
      enddo

      if (debug) then 
         wlu = 0
         call dump_iyc_arrays(wlu, curve_num, cusip_arr, flg_arr,
     >        sflg_arr,in_ytms, arr_size)
      endif

      end

c*************************************************************************
C     Get the interplated iyc curve at the standand maturities and copy
C     it to the common area needed by the oas calculation. 
c*************************************************************************

      subroutine fir_get_iyc_11pt_curve(
     >     curve_num,     curve_dt,      bidask,      pcs_method,
     >     pcs_number,    cusip_arr,     flg_arr,     sflg_arr,
     >     curve,         arr_size,      curve_title, curve_freq,
     >     rcode)

      include '/bbinc/d1/g/fetchcrv2_constants.inc' 
      include '/bbinc/l1/g/oasyc.inc' 
c     input
      integer*4   curve_num        
      integer*4   pcs_method 
      integer*4   pcs_number
      integer*2   curve_dt(3)      
      character   bidask*2    
      integer*4   arr_size
     
c     output
      real*8      curve(*)         
      character   curve_title*(*)  
      integer*2   curve_freq       
      character*8 cusip_arr(*)
      integer*2   flg_arr(*)
      integer*2   sflg_arr(*)
      integer*4   rcode     
c     functions 
      integer*2   int2

      if (arr_size .lt. STD_CRV_PTS) then 
         rcode = CRV_ERROR_SMALL_SIZE_11PTCURVE
         return
      endif

      call fir_get_interplated_iyc_curve(
     >     curve_num,     curve_dt,      bidask,         pcs_method,
     >     pcs_number,    yc11_ytms,     STD_CRV_PTS,    cusip_arr,
     >     flg_arr,       sflg_arr,      curve,          curve_title,
     >     curve_freq,    rcode)

      if (rcode .ne. 0) return

c     both yc11_time and yc11_ytms are the standard maturity arrays.
C     The only difference between them is yc11_time is real*4 while
C     yc11_ytms is real*8. yc11_time has to be used here, because, it
C     will be copied to OASYCTIM, which is real*4 array. 
      call copy_curve_to_oasycm(curve, yc11_time, curve_freq, 
     >     int2(STD_CRV_PTS), rcode)

      end

*************************************************************************

      subroutine  set_yield_convention(curve_num, yldcnv)
      integer*4   curve_num
      integer*2   yldcnv

c     Janpanese curve I18 may be quoted as simple yield, depending on
C     the user's default (COVR). However, every bond in this curve is 
c     semiannual, so that the actual frequency is 2. 

C     RCS id (do not change)
      CHARACTER*80 RCSid
      DATA RCSid /
     +'$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     +$'/

      if (curve_num .eq. 18) then   
         yldcnv = 2   ! use the semiannual compounding for japanese curve. 
      else 
         yldcnv = 1   ! default 
      endif

      end

*************************************************************************

      logical function putl_iyc_curve_switch()
      logical fir_check_putl_byte


C     RCSId was inserted for Ident:
      CHARACTER*80 RCSId
      DATA RCSId /
     +'$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     + $'/

      putl_iyc_curve_switch=fir_check_putl_byte(596,
     >     'putl_iyc_curve_switch', 0)
      end

*************************************************************************

      logical function fir_init_curve_freq()
      logical fir_check_putl_byte


C     RCSId was inserted for Ident:
      CHARACTER*80 RCSId
      DATA RCSId /
     +'$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     + $'/

      fir_init_curve_freq=fir_check_putl_byte(615,
     >     'fir_init_curve_freq', 0)
      end

*************************************************************************

      subroutine copy_curve_to_oasycm(incurve, inmty, infreq, 
     >     incnt, rcode)
      include '/bbinc/d1/g/fetchcrv2_constants.inc'
      include '/bbinc/l1/g/oasyc.inc' 
c     input
      integer*2  incnt, infreq
      real*8     incurve(*)
      real*4     inmty(*)
c     output
      integer*4  rcode

C     RCS id (do not change)
      CHARACTER*80 RCSid
      DATA RCSid /
     +'$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     +$'/

c     32 is the array size of OASYCURV
      if (incnt .gt. 32) then 
         rcode = CRV_ERROR_SMALL_SIZE_OASYCM 
         return
      endif
         
      OASYCNT = incnt
      OASYCFRQ = infreq
      call moveit(inmty, OASYCTIM, OASYCNT)
      call moveit(incurve,OASYCURV,OASYCNT*2)         

      end

c*************************************************************************

      integer*2 function fir_get_curve_freq(curve_num, freq_arr,
     >     arr_size, rcode)
      integer*2    freq_arr(*)
      integer*4    curve_num, arr_size, rcode
      
c     local
      integer*2    curve_freq, ii
      

C     RCS id (do not change)
      CHARACTER*80 RCSid
      DATA RCSid /
     +'$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     +$'/

      curve_freq=0
      rcode=0
      do ii = arr_size, 1, -1
         if (freq_arr(ii).ne.0) then
            curve_freq = freq_arr(ii)
            goto 11
         endif
      enddo

11     if (curve_freq .eq. 0) then 
          type *,'emsg:fir_get_curve_freq:<<FIR>>curve_freq=0 crvnum=',
     >         curve_num
         rcode=20020531
      endif
      fir_get_curve_freq=curve_freq

      end
      
c*************************************************************************
c     Several different arrays in the common area are available after
C     the iycinfo call: ycrvbid, ycrvask and ycrvylds. The first two are
C     filled only when bidask is set to ' '  in the iycinfo2 or
C     iycinfo4 calls. The last is filled only when bidask is set to  B
C     or A (anything other than ' ' or B is treated the same as A).
c*************************************************************************

      subroutine copy_iyc_curve_from_common(
     >     iyc_ba,   req_ba,      iycflg,   iycsflg,
     >     iyccurve, iyc_ytms,    arr_size, rcode)

      include '/bbinc/l1/g/oasyc.inc' 
      include '/bbinc/d1/g/yldcomn.inc'
      include '/bbinc/d1/g/fetchcrv2_constants.inc'
c     input
      character*(*) iyc_ba   ! the bidask used in iycinfo call
      character*(*) req_ba   ! the bidask requested
      integer*4     arr_size
c     output
      real*8        iyccurve(*)
      real*8        iyc_ytms(*)
      integer*2     iycflg(*)
      integer*2     iycsflg(*)
      integer*4     rcode

c     local
      character     ba   
      integer*4     memsize

      if (arr_size .lt. IYC_CRV_PTS) then 
         rcode = CRV_ERROR_SMALL_SIZE_COMMON
         return
      endif

      if (iyc_ba .ne. req_ba .and. iyc_ba .ne. ' ') then 
c     if the iyc_bidask is not ' ', it should be the same as req_bidask
         type *,'emsg:copy_iyc_curve_from_common:<<FIR>>iyc_ba=',
     >        iyc_ba,' req_ba=', req_ba
         rcode = 5559
         return
      endif

      memsize = IYC_CRV_PTS*4
      call moveit2(yc15_ytms, iyc_ytms, memsize)
      call moveit2(ycrvflg,   iycflg,   IYC_CRV_PTS)
      call moveit2(ycrvsflg,  iycsflg,  IYC_CRV_PTS)

      ba=req_ba(1:1)
      if (iyc_ba .eq. ' ') then 
         if (ba .eq. 'B' .or. ba .eq. 'b') then
            call moveit2(ycrvbid, iyccurve,  memsize)
         else if (ba .eq. 'M' .or. ba .eq. 'm') then 
            call get_mid_from_bid_ask(ycrvbid, ycrvask, IYC_CRV_PTS,
     >           iyccurve)
         else  ! to be consistent with iycinfo 
            call moveit2(ycrvask, iyccurve,  memsize)
         endif
      else  ! must be the case: iyc_ba==req_ba
         call moveit2(ycrvylds, iyccurve,  memsize)
      endif

      end

c*************************************************************************

      subroutine get_mid_from_bid_ask(bid_arr, ask_arr, arr_size,
     >           mid_arr)
c     input
      integer*4   arr_size
      real*8      bid_arr(*), ask_arr(*)
c     output
      real*8      mid_arr(*)
c     local 
      integer*4   i
      
      do i = 1, arr_size
         mid_arr(i) = (bid_arr(i)+ask_arr(i))/2.0
      enddo

      end

c*************************************************************************
c     set input parameters for calling iycinfo4. 
c*************************************************************************

      subroutine set_parameters_for_iycinfo4(
     >     pcs_method,       pcs_number,       bidask_in,
     >     prcnum,           tscode,           i2sec,
     >     setcnv_out,       bidask_out,       rcode,
     >     error_code)

      include '/bbinc/d1/g/oas_statistics.inc'
      include '/bbinc/d1/g/fetchcrv2_constants.inc'
c     input
      integer*4      pcs_method
      integer*4      pcs_number
      character*(*)  bidask_in   
c     output
      character*(*)  bidask_out   
      integer*2      i2sec, setcnv_out
      integer*4      prcnum, tscode, rcode
      integer*4      error_code

      error_code = 0
      tscode = 0
      i2sec = 1

      if (bidask_in .eq. 'M' .or. bidask_in .eq. 'm') then 
         bidask_out = ' '  ! to allow mid calulation. 
      else
         bidask_out = bidask_in  
c     don't set bidask_out to ' ' for other cases  because it  may
C     change I52 undesirably. Some drqs was entered for this kind of
C     change, even though I can't recall the drqs number.  3/3/2003
      endif


c     prcnum .ne. 0 allows you to override the default pricing
C     behavior of the curve.  If prcnum == 0, iycinfoX() will use the
C     user's PCS list for pricing the curve.  If prcnum .ne. 0, and rcode
C     is set to something other than 0, the value of rcode will be
C     used as the PCS number.  If prcnum .ne. 0 and rcode == 0, the
C     curve will be forced to use BGN pricing. Comment from a message by:
C     Shirley Monroe  12/17/2002.

      if (pcs_method .eq. PCS_USE_PRICING_NUMBER) then 
         prcnum = 1
         rcode = pcs_number
         setcnv_out = 1
      else if (pcs_method .eq. PCS_FORCE_BGN) then 
         prcnum = 0
         rcode  = 0
         setcnv_out = 2
c     force using source BGN (fall to bfv if bgn is not available)
      else if (pcs_method .eq. PCS_USE_DEFAULT) then
         prcnum = 0
         rcode  = 0
         setcnv_out = 1
c     use standard settlement without forcing source BGN
      else 
         error_code = CRV_ERROR_PCS_METHOD_UNKNOWN  
      endif

      end

c*************************************************************************

      logical function fixed_pcs_curve_needed(fixed_pcs, curve_num)
c     input
      logical        fixed_pcs
      integer*4      curve_num     
c     local     
      character*20   crvid
      integer*4      cvtlen
c     function
      character      itoc*12
      logical        curve_is_fixed_pcs_iyc

      crvid = 'I'//itoc(curve_num, cvtlen)
      fixed_pcs_curve_needed = fixed_pcs .and.
     >     curve_is_fixed_pcs_iyc(crvid)

      end

c*************************************************************************

      logical function putl_get_swap_curve()
      logical fir_check_putl_byte


C     RCSId was inserted for Ident:
      CHARACTER*80 RCSId
      DATA RCSId /
     +'$Id: fetchcrv2.f,v 1.19 2003/03/06 19:39:13 zhuang Exp zhuang $
     + $'/

      putl_get_swap_curve=fir_check_putl_byte(923,
     >     'putl_get_swap_curve', 0)
      end


c*************************************************************************

      subroutine fir_get_swap_11pt_curve(curve_num, curve_dt, bidask,
     >      curve, curve_title, curve_freq, rcode)

      include '/bbinc/l1/g/oasyc.inc' 
c     input
      integer*4 curve_num     
      integer*2 curve_dt(3)   
      character bidask*2      
c     output
      real*8    curve(*)          
      character curve_title*(*)   
      integer*2 curve_freq        
      integer*4 rcode             
c     local
      integer*4 termidx(11)
      real*8    coups(11)
      integer*2 curve_pts     
      data      termidx/4, 7, 9, 11, 13, 14, 15, 17, 20, 24, 26/

      curve_pts = 11
      call oas_getswyc(curve_num, curve_dt, curve_pts, termidx,
     +     curve_title, coups, curve_freq, bidask, rcode)
      if (rcode.ne.0) then
         type *,'emsg:fir_get_swap_11pt_curve:oas_getswyc rc=', rcode,
     >        ' curve_num=', curve_num
         rcode = 5551
         return
      endif
      call condition_curve(coups, yc11_ytms, int(curve_pts),
     +     curve, yc11_ytms, int(curve_pts), rcode)
      if (rcode.ne.0) then
         type *,'emsg:fir_get_swap_11pt_curve:condition_curve rc=',
     >        rcode,' swyc curve_num=', curve_num, ' curve_pts=',
     >        curve_pts
         rcode = 5552
         return
      endif

      call copy_curve_to_oasycm(curve, yc11_time, curve_freq, 
     >     curve_pts, rcode)
      
      end


c*************************************************************************
c     oasf_curve (or fnma_curve) is the unique such curve currently.
C     fhlmc_curve will be added soon. All these curve(s) are IYC, have a
C     fixed pricing source and have  adjustment shifts for converting
C     it into par curve. Swap curves in aoas will also have a fixed
C     pricing source, but have no shift, and so do not belong to this
C     catagory. 
c*************************************************************************

      logical function curve_is_fixed_pcs_iyc(crvid)
      include '/bbinc/d1/g/oas_statistics.inc'
c     input
      character*(*)   crvid
c     function
      logical         putl_iyc_curve_switch

      curve_is_fixed_pcs_iyc = crvid .eq. fnma_curve 
     >     .or.  (crvid .eq. fhlmc_curve .and. putl_iyc_curve_switch())

      end


c*************************************************************************

      character*1 function get_fixed_pcs_bidask()
      
      get_fixed_pcs_bidask='A'

      end

c*************************************************************************

      integer*2 function  get_fixed_pcs_i2setcnv(crvid, rcode)
      include '/bbinc/d1/g/oas_statistics.inc'
      
c     input
      character*(*)   crvid
      integer*4       rcode

      rcode = 0
      if (crvid .eq. fnma_curve) then 
         get_fixed_pcs_i2setcnv = 1
c     use standard settlement without forcing source BGN

      else if (crvid .eq. fhlmc_curve) then 
         get_fixed_pcs_i2setcnv = 2
c     force using source BGN (fall to bfv if bgn is not available)

      else
         get_fixed_pcs_i2setcnv = -1
         rcode = 5572
      endif
            
      end
      
c*************************************************************************

      character*1 function  get_fixed_pcs_csetcnv(crvid, rcode)
c     input
      character*(*)   crvid
      integer*4       rcode
c     local 
      integer*2       setcnv
c     function
      integer*2       get_fixed_pcs_i2setcnv

      rcode = 0
      setcnv = get_fixed_pcs_i2setcnv(crvid, rcode)
      if (rcode .ne. 0) then 
         get_fixed_pcs_csetcnv = 'E'   ! setting to any character is fine
         return
      endif

      if (setcnv .eq. 1)  then 
         get_fixed_pcs_csetcnv = 'N'
      else if (setcnv .eq. 2) then 
         get_fixed_pcs_csetcnv = 'Y'
      else
         get_fixed_pcs_csetcnv = 'E'
         rcode = 5571
      endif

      end

c*************************************************************************

      integer*4  function get_fixed_pcs_prcnum(crvid, rcode)
      include '/bbinc/d1/g/oas_statistics.inc'
c     input
      character*(*)   crvid
      integer*4       rcode
      
      rcode = 0
      if (crvid .eq. fnma_curve) then 
         get_fixed_pcs_prcnum = 1
c     prcnum .ne. 0 allows you to override the default pricing
C     behavior of the curve.  If prcnum == 0, iycinfoX() will use the
C     user's PCS list for pricing the curve.  If prcnum .ne. 0, and rcode
C     is set to something other than 0, the value of rcode will be
C     used as the PCS number.  If prcnum .ne. 0 and rcode == 0, the
C     curve will be forced to use BGN pricing. Comment from a message by:
C     Shirley Monroe  12/17/2002.

      else if (crvid .eq. fhlmc_curve) then 
         get_fixed_pcs_prcnum = 0
      else
         rcode = 5570
      endif

      end

c*************************************************************************

      integer*4 function get_fixed_pcs_prc_source(crvid, rcode)
      include '/bbinc/d1/g/oas_statistics.inc'
c     input
      character*(*)   crvid
      integer*4       rcode

      rcode = 0
      if (crvid .eq. fnma_curve) then 
         get_fixed_pcs_prc_source = fnma_pricing_source
      else if (crvid .eq. fhlmc_curve) then 
         get_fixed_pcs_prc_source = 0
      else
         rcode = 5560
      endif

      end

c*************************************************************************
      
      logical   function  is_fnma_curve(crvid)
      include '/bbinc/d1/g/oas_statistics.inc'
      character   crvid*(*)

      is_fnma_curve = crvid .eq. fnma_curve

      end


c***********************************************************************

      logical function is_bond_curve(crvid, cusip_arr, flg_arr,
     >     arr_size)
      INCLUDE '/bbinc/c1/g/products.inc'
c     input
      character    crvid*(*)
      character*8  cusip_arr(*)
      integer*2    flg_arr(*)
      integer*4    arr_size
c     local       
      integer*2    i, flag, num_bonds
c     function
      logical      putl_iyc_curve_switch
      logical      curve_has_calc_shifts

c     When putl 596 is off, the old code is used and so only I267, I25,
C     and I111 are bond curves.
      if (.not. putl_iyc_curve_switch()) then 
         is_bond_curve = curve_has_calc_shifts(crvid)
         return
      endif

      if (crvid(1:1) .ne. 'I') then 
         is_bond_curve = .false.
         return
      endif
      
      num_bonds = 0
      do i=1, arr_size
         if (cusip_arr(i) .ne. ' ') then 
            num_bonds = num_bonds + 1
            flag = flg_arr(i)
            if (flag .ne. GOVT .and. flag .ne. CORP) then 
               is_bond_curve = .false. 
               return
            endif
         endif
      enddo
               
c     Exclude curves that has  less than 3 securities
      if (num_bonds .lt. 3) then 
         is_bond_curve = .false.
      else
         is_bond_curve = .true.
      endif

      end

c***************************************************************************

      subroutine  dump_iyc_arrays(wlu, curve_num, cusip_arr, flg_arr,
     >     sflg_arr,in_ytms, arr_size)
c     input
      integer*4    wlu, curve_num, arr_size
      integer*2    flg_arr(*), sflg_arr(*)
      real*8       in_ytms(*)
      character*8  cusip_arr(*)
c     local 
      integer*4    i

      write(wlu, *) 'curve_num=', curve_num
      do i = 1, arr_size
         write(wlu, '(a5, i4, a12, a10, a12, i4, a12, i4, a8, f5.2)')      
     >        'i=', i, ' cusip_arr=', cusip_arr(i), ' flg_arr=',
     >        flg_arr(i), '  sflg_arr=', sflg_arr(i), ' year=',
     >        in_ytms(i)
      enddo

      end

c***********************************************************************

      subroutine init_iyc_arrays(cusip, flg, sflg, ytms, size)
c     input
      character*8    cusip(*)
      integer*2      flg(*), sflg(*), ytms(*)
      integer*4      size

c     local
      integer*2      i

      do i = 1, size
         cusip(i) = ' '
         flg(i) = 0
         sflg(i) = 0
         ytms(i) = 0
      enddo

      end

c*************************************************************************
c     find the indices in a set of real numbers that matches the subset
C     (within the tolerance).  Returns error when the subset is not a
C     real subset of the set. It is assumed that both set and subset
C     arrays do not have idential (with tolerance) elements.  Otherwise,
C     the indices output may have idential elements. 
c*************************************************************************

      subroutine fir_matching_subset(
     >     set,          set_size,        subset,
     >     subset_size,  tolerance,       indices,
     >     rcode)
c     input
      real*8     set(*), subset(*)
      real*8     tolerance
      integer*4  set_size, subset_size
c     output
      integer*4  indices(*), rcode
c     local 
      integer*4  i, j
      logical    found

      rcode = 0

      do i = 1, subset_size
         found = .false. 
         do j = 1, set_size
            if (dabs(subset(i) - set(j)) .lt. tolerance) then 
               indices(i) = j
               found = .true.
            endif
         enddo
         if (.not. found)  then 
            rcode = 1
            return
         endif
      enddo

      end


c*************************************************************************

      subroutine  setcnv_2_pcs_method(setcnv, pcs_method)
      include '/bbinc/d1/g/fetchcrv2_constants.inc'
c     input 
      integer*2    setcnv
c     output
      integer*4    pcs_method
      
      if (setcnv .eq. 2) then 
         pcs_method = PCS_FORCE_BGN
      else
         pcs_method = PCS_USE_DEFAULT
      endif

      end
\")
  (insert-buffer (other-buffer))
  (save-buffer)
)


(other-buffer)


