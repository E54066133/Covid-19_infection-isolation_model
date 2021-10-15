PROGRAM infection
!IMPLICIT none

   INTEGER  :: infecton_seed
   LOGICAL  :: again
   INTEGER  :: infecton_vector(4)
   REAL     :: infection_link(4)
   INTEGER  :: infection_data(100)
   REAL     :: temp_recieve(4,100)    !暫存外面的資料  OK
   INTEGER  :: temp_save(4,100)       !暫存外面的資料  OK
   INTEGER  :: vector(4,100)          !要從外面讀資料  OK
   REAL     :: link(4,100)            !要從外面讀資料  OK
   REAL     :: edge(100)              !要從外面讀資料  OK
   INTEGER  :: isolate(7)             !要從外面讀資料  OK
   INTEGER  :: final_infection
   INTEGER  :: result(100)
   INTEGER  :: resultcount
   REAL     :: average
   INTEGER  ::h,i,j,k,l,m,n,o,q,r,s,t,u,v,w,y,z


   CALL random_seed
   again=.TRUE.
   final_infection=0
   result=0.0

   !讀測資
   OPEN(unit=14,file="test_data.f90",status="OLD")
   DO w=1,100
      DO t=1,4
         READ(14,*)temp_recieve(t,w)
      ENDDO
   ENDDO

   !解讀測資
   DO v=1,100
     DO u=1,4
       temp_save(u,v)=int(temp_recieve(u,v)*10)   !取出下一步要用的連結度(整數型態) ex.0.7094 => 7
       link(u,v)=real(temp_save(u,v))*0.1         !連結度=(整數型態)*0.1 ex.7 => 0.7                    !連結度
       temp_save(u,v)=int(temp_recieve(u,v)*10000)!data換成整數型態      ex.0.7094 => 7094
       vector(u,v)=MOD(temp_save(u,v),1000)       !指標=7094除以1000的餘數 ex.7094/1000=7.....「94」        !人類編號
     ENDDO
   ENDDO



   !讀isolate()資料               !隔離誰
   OPEN(unit=16,file="test_isolate.f90",status="OLD")
   DO r=1,7
      READ(16,*)isolate(r)
   ENDDO

   !讀edge()資料                 !抵抗力
   OPEN(unit=18,file="edge_data.f90",status="OLD")
   DO s=1,100
      READ(18,*)edge(s)
   ENDDO



   !處理隔離
   DO q=1,7
      edge(isolate(q))=100       !激發值大到不可能被感染
   ENDDO

   DO z=1,100
     again=.TRUE.
     final_infection=0
     infecton_vector=0
     infection_link=0
   ENDDO


   DO i=1,100
      infection_data(i)=0        !infection(i)==0 代表未被感染
   ENDDO



   !隨機挑選初始帶源者(隔離者不能選)
   DO j=1,3
      ok:DO WHILE(.TRUE.)
         CALL random_number(p)
         infection_seed=int(p*100)
         DO y=1,7
            IF(infection_seed/=isolate(y))EXIT ok
         ENDDO
      ENDDO ok
      infection_data(infection_seed)=1
   ENDDO


   !感染模型
   DO WHILE(again)
       again=.FALSE.
       DO k=1,100
          IF(infection_data(k)==1)THEN        !infection(k)==1 代表已感染但還未傳播
             DO l=1,4
                infecton_vector(l)=vector(l,k)
                infection_link(l) =link(l,k)
                IF(infection_link(l)>=edge(infecton_vector(l))&
                &.AND.infection_data(infecton_vector(l))==0)THEN
                   infection_data(infecton_vector(l))=1
                ENDIF
             ENDDO
             infection_data(k)=2             !infection(k)==2 代表已處理散播可能
          ENDIF
          DO n=1,100
             IF(infection_data(n)==1)THEN
                again=.TRUE.
             ENDIF
          ENDDO
       ENDDO
   ENDDO


   !計算最後感染人數
   DO o=1,100
      IF(infection_data(o)==2)THEN
         final_infection=final_infection+1
      ENDIF
   ENDDO

   result(z)= final_infection
   ENDDO

   resultcount=0
   DO h=1,100
     resultcount=resultcount+result(h)
     average=REAL(resultcount/100)
   ENDDO

   WRITE(*,*)"RESULT IS : ",average,"%"


END PROGRAM infection