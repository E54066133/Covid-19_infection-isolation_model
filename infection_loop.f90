PROGRAM infection
!IMPLICIT none

   INTEGER  :: infecton_seed
   LOGICAL  :: again
   INTEGER  :: infecton_vector(4)
   REAL     :: infection_link(4)
   INTEGER  :: infection_data(100)
   REAL     :: temp_recieve(4,100)    !�Ȧs�~�������  OK
   INTEGER  :: temp_save(4,100)       !�Ȧs�~�������  OK
   INTEGER  :: vector(4,100)          !�n�q�~��Ū���  OK
   REAL     :: link(4,100)            !�n�q�~��Ū���  OK
   REAL     :: edge(100)              !�n�q�~��Ū���  OK
   INTEGER  :: isolate(7)             !�n�q�~��Ū���  OK
   INTEGER  :: final_infection
   INTEGER  :: result(100)
   INTEGER  :: resultcount
   REAL     :: average
   INTEGER  ::h,i,j,k,l,m,n,o,q,r,s,t,u,v,w,y,z


   CALL random_seed
   again=.TRUE.
   final_infection=0
   result=0.0

   !Ū����
   OPEN(unit=14,file="test_data.f90",status="OLD")
   DO w=1,100
      DO t=1,4
         READ(14,*)temp_recieve(t,w)
      ENDDO
   ENDDO

   !��Ū����
   DO v=1,100
     DO u=1,4
       temp_save(u,v)=int(temp_recieve(u,v)*10)   !���X�U�@�B�n�Ϊ��s����(��ƫ��A) ex.0.7094 => 7
       link(u,v)=real(temp_save(u,v))*0.1         !�s����=(��ƫ��A)*0.1 ex.7 => 0.7                    !�s����
       temp_save(u,v)=int(temp_recieve(u,v)*10000)!data������ƫ��A      ex.0.7094 => 7094
       vector(u,v)=MOD(temp_save(u,v),1000)       !����=7094���H1000���l�� ex.7094/1000=7.....�u94�v        !�H���s��
     ENDDO
   ENDDO



   !Ūisolate()���               !�j����
   OPEN(unit=16,file="test_isolate.f90",status="OLD")
   DO r=1,7
      READ(16,*)isolate(r)
   ENDDO

   !Ūedge()���                 !��ܤO
   OPEN(unit=18,file="edge_data.f90",status="OLD")
   DO s=1,100
      READ(18,*)edge(s)
   ENDDO



   !�B�z�j��
   DO q=1,7
      edge(isolate(q))=100       !�E�o�Ȥj�줣�i��Q�P�V
   ENDDO

   DO z=1,100
     again=.TRUE.
     final_infection=0
     infecton_vector=0
     infection_link=0
   ENDDO


   DO i=1,100
      infection_data(i)=0        !infection(i)==0 �N���Q�P�V
   ENDDO



   !�H���D���l�a����(�j���̤����)
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


   !�P�V�ҫ�
   DO WHILE(again)
       again=.FALSE.
       DO k=1,100
          IF(infection_data(k)==1)THEN        !infection(k)==1 �N��w�P�V���٥��Ǽ�
             DO l=1,4
                infecton_vector(l)=vector(l,k)
                infection_link(l) =link(l,k)
                IF(infection_link(l)>=edge(infecton_vector(l))&
                &.AND.infection_data(infecton_vector(l))==0)THEN
                   infection_data(infecton_vector(l))=1
                ENDIF
             ENDDO
             infection_data(k)=2             !infection(k)==2 �N��w�B�z�����i��
          ENDIF
          DO n=1,100
             IF(infection_data(n)==1)THEN
                again=.TRUE.
             ENDIF
          ENDDO
       ENDDO
   ENDDO


   !�p��̫�P�V�H��
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