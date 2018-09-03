FC=ifort
FLAGS_IFORT= -CB -traceback -g 
FLAGS_GCC= -O2
# コンパイルのために順番が大事。下層ほど先に書く。 
SRCS=\
     global_parameters.f90 \
     subroutines.f90 \
     ManyBodyGravity.f90
OBJS=$(SRCS:.f90=.o)
#########################
PROG=manybodygrav.exe

#.SUFFIXES : .o .f90 # .oを作るときは必ず.f90から作るよ
.SUFFIXES : .f90 # .oを作るときは必ず.f90から作るよ
 
all:$(PROG) 

$(PROG): $(OBJS) $(OBJ_MAIN)
ifeq ($(FC),gfortran)
	$(FC) -O2 $(FLAGS_GCC) -o $@ $(OBJS) $(LIB)
else
	$(FC) $(FLAGS_IFORT) -o $@ $(OBJS) $(LIB)
endif

# moduleをコンパイルするときの依存性を解消
%.o: %.f90
ifeq ($(FC),gfortran)
	$(FC) -c $<
else
	$(FC) $(FLAGS_IFORT) -c $<
endif
%.mod: %.f90 %.o
	@true

# moduleの依存性
subroutines.o: \
  global_parameters.o


.PHONY: clean
clean:
	mv $(PROG) $(PROG).bak; rm -f *.o *.mod core 
