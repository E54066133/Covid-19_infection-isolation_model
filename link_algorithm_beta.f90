PROGRAM mid_test_link_algorithm

   INTEGER   :: i,j,k,l,m,n,o,r,kkk,mmm
   INTEGER   :: link_count
   INTEGER   :: default_x
   REAL      :: max_link,second_max_link,third_max_link,forth_max_link
   REAL      :: fifth_max_link,sixth_max_link,seventh_max_link,eighth_max_link
   REAL      :: input_data(4,100)
   REAL      :: edge(100)
   INTEGER   :: temp(4,100)
   INTEGER   :: maybe_infection(100)
   INTEGER   :: infection_susceed(100)
   INTEGER   :: vector(4,100)
   REAL      :: link(4,100)
   REAL      :: sum_link(100)
   INTEGER   :: link_answer_first(100),link_answer_final(100)
   INTEGER   :: vector_chain(100)
   INTEGER   :: goal
   INTEGER   :: temp_vector



   OPEN(unit=12,file="test_data.f90",status="unknown")
   OPEN(unit=14,file="edge_data.f90",status="unknown")
   OPEN(unit=13,file="test_isolate.f90",status="unknown")
   
   CALL random_seed


   !讀進data
   DO j=1,100
     DO i=1,4
         READ(12,*)input_data(i,j)
     ENDDO
   ENDDO
   
   DO j=1,100
         READ(14,*)edge(j)
   ENDDO



   !解讀data
   DO j=1,100
     DO i=1,4
       temp(i,j)=INT(input_data(i,j)*10)         !取出下一步要用的連結度(整數型態) ex.0.7019 => 7
       link(i,j)=REAL(temp(i,j))*0.1             !連結度=(整數型態)*0.1 ex.7 => 0.7
       temp(i,j)=INT(input_data(i,j)*10000)      !data換成整數型態      ex.0.7019 => 7019
       vector(i,j)=MOD(temp(i,j),1000)           !指標=7019除以1000的餘數 ex.7019/1000=7.....「19」
     ENDDO
   ENDDO





   !找出可能感染者和可以傳播出去的人
   DO m=1,100
      maybe_infection(m)=0              !先清空 maybe_infection()
      infection_susceed(m)=0            !先清空 infection_susceed()
   ENDDO

   DO l=1,100
      DO k=1,4
         IF(edge(vector(k,l))<=link(k,l))THEN
            maybe_infection(vector(k,l))= maybe_infection(vector(k,l))+1
            infection_susceed(l)=infection_susceed(l)+1
         ENDIF
      ENDDO
   ENDDO


   !計算傳播鏈長度
   DO kkk=1,100
      vector_chain(kkk)=0               !先清空 vector_chain()
   ENDDO
   DO mmm=1,100
      goal=mmm
      temp_vector=vector(1,mmm)
      vector_chain(mmm)=vector_chain(mmm)+1
      DO WHILE(.TRUE.)
         temp_vector=vector(1,temp_vector)
         vector_chain(mmm)=vector_chain(mmm)+1
         IF(temp_vector==goal)EXIT
         IF(vector_chain(mmm)>=7)EXIT
      ENDDO
   ENDDO


   !依據條件選出要隔離的人
   DO o=1,100
      link_answer_final(o)=0                 !清空link_answer_final()
   ENDDO
   link_count=0
   DO o=1,100
      IF(infection_susceed(o)==4 .AND. maybe_infection(o)>0 .AND. vector_chain(o)>=7)THEN
         link_answer_final(o)=1
         link_count=link_count+1
         IF(link_count>=7)EXIT
      ENDIF
   ENDDO
   IF(link_count<7)THEN                      !怕link_answer_first()==1
      DO o=1,100                             !沒有7個隔離值所以找==2
         IF(infection_susceed(o)==3 .AND.maybe_infection(o)>0 .AND. vector_chain(o)>=7)THEN
            link_answer_final(o)=1
            link_count=link_count+1
            IF(link_count>=7)EXIT
         ENDIF
      ENDDO
   ENDIF
   IF(link_count<7)THEN                      !怕link_answer_first()==2
      DO o=1,100                             !沒有7個隔離值所以找==3
         IF(infection_susceed(o)==2 .AND.maybe_infection(o)>0 .AND. vector_chain(o)>=7)THEN
            link_answer_final(o)=1
            link_count=link_count+1
            IF(link_count>=7)EXIT
         ENDIF
      ENDDO
   ENDIF
   IF(link_count<7)THEN                      !怕link_answer_first()==3
      DO o=1,100                             !沒有7個隔離值所以找==4
         IF(infection_susceed(o)==1 .AND.maybe_infection(o)>0 .AND. vector_chain(o)>=7)THEN
            link_answer_final(o)=1
            link_count=link_count+1
            IF(link_count>=7)EXIT
         ENDIF
      ENDDO
   ENDIF

   IF(link_count<7)THEN                       !怕沒有7個隔離值的default
      DO WHILE(.TRUE.)
         CALL random_number(x)
         IF(x>0.01)THEN
            default_x=INT(x*100)
            IF(link_answer_final(default_x)/=1)THEN
               link_answer_final(default_x)=1
               link_count=link_count+1
               IF(link_count>=7)EXIT
            ENDIF
         ENDIF
      ENDDO
   ENDIF



   DO r=1,100
      IF (link_answer_final(r)==1)THEN
         WRITE(*,*)"Answer is",r           !link演算法 答案
         WRITE(13,*)r
      ENDIF
   ENDDO




STOP
END PROGRAM mid_test_link_algorithm