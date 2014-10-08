!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at
! https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

program cubedsphere
!================================================
! Author: Paul Slavin <slavinp@cs.man.ac.uk>
! Date: 21 August 2014
! Modified 1 October 2014
!
! Fortran implementation of Mike Rezny's
! cubedsphere2.m octave code. Populates FaceID,
! Faces and Vertices arrays with equivalent
! values for arbitrary N.
!
! Specifying N = 29224 needs about 126GB of memory.
!
!================================================
use, intrinsic :: iso_fortran_env, only : output_unit
use constants_mod,   only : l_def, i_def, r_def, str_def, pi
!================================================
implicit none

integer   (kind=i_def)                :: N
integer   (kind=i_def)                :: argc
character (len=str_def), dimension(2) :: argv
logical   (kind=l_def)                :: output_verts, output_faces, silent
!================================================

output_verts = .FALSE.
output_faces = .FALSE.
silent       = .FALSE.    ! Useful for runtime/memory profiling

argc = command_argument_count()
if(argc.ne.2) then
    write(output_unit, "(A)") "Usage: cubedsphere <n_divs> <output_spec>"
    write(output_unit, "(A)") "  n_divs      : number of divisions per edge"
    write(output_unit, "(A)") "  output_spec:"
    write(output_unit, "(A)") "    'A' output Vertices and Faces"
    write(output_unit, "(A)") "    'V' output only Vertices"
    write(output_unit, "(A)") "    'F' output only Faces"
    write(output_unit, "(A)") "    'S' silent, calculate vertices and faces, but no output"
    write(output_unit, "(A)") "        useful for performance analysis"

    call exit(1)
endif
call get_command_argument(1, argv(1))
call get_command_argument(2, argv(2))
read(argv(1), *) N
select case(argv(2))
    case("A")
        output_verts = .TRUE.
        output_faces = .TRUE.
    case("V")
        output_verts = .TRUE.
    case("F")
        output_faces = .TRUE.
    case("S")
        output_verts = .TRUE.
        output_faces = .TRUE.
        silent = .TRUE.
    case default
        write(output_unit, "(A,A1,A)") "Invalid output specifier '", argv(2), "'"
        call exit(1)
end select

if(output_verts) then
    call write_vertices(N)
endif

if(output_faces) then
    call write_faces(N)
endif

!================================================
contains
!================================================
subroutine merge_vertices(V, Fx, Fy, Fz)
implicit none

real (kind=r_def), dimension(:,:), intent(INOUT) :: V
real (kind=r_def), dimension(:,:), intent(IN)    :: Fx, Fy, Fz

V = reshape( (/ reshape(Fx, (/ size(Fx) /)), &
            reshape(Fy, (/ size(Fy) /)), &
            reshape(Fz, (/ size(Fz) /)) /), &
            (/ size(Fx), 3 /) &
           )

end subroutine merge_vertices
!================================================
subroutine merge_faces(F, V1, V2, V3, V4)
implicit none

integer (kind=i_def), dimension(:,:), intent(INOUT) :: F
integer (kind=i_def), dimension(:,:), intent(IN)    :: V1, V2, V3, V4

F = reshape( (/ reshape(V1, (/ size(V1) /)), &
            reshape(V2, (/ size(V2) /)), &
            reshape(V3, (/ size(V3) /)), &
            reshape(V4, (/ size(V4) /)) /), &
            (/ size(V1), 4 /) &
           )

end subroutine merge_faces
!================================================
subroutine write_vertices(N)
implicit none        

integer (kind=i_def), intent(in)           :: N          ! divisions per edge

integer (kind=i_def)                       :: i, incsteps
integer (kind=i_def)                       :: Fstart, Flen, Fend, Estart, Elen

real (kind=r_def), dimension(:,:), pointer :: X, Y, Z, S
real (kind=r_def), dimension(8)            :: Vx, Vy, Vz ! Corners
real (kind=r_def), dimension(:,:), pointer :: Ex, Ey, Ez ! Edges, minus corners, dim(N-1,12)
real (kind=r_def), dimension(:,:), pointer :: Fx, Fy, Fz ! Faces, interior points,
                                                         ! not E or C  dim(N-1, N-1)
real (kind=r_def), dimension(:,:), pointer :: Vertices   ! Verts(:,C,E,F) dim(6*N*N+2, 3)

real (kind=r_def), dimension(:), pointer   :: theta      ! dim(N+1)
real (kind=r_def)                          :: inc

allocate(theta(N+1))

Vx = 0.0_r_def; Vy = 0.0_r_def; Vz = 0.0_r_def

Estart = 9
Elen   = N-1
Flen   = (N-1)*(N-1)

inc = 0.5_r_def * pi / N
theta = (/ (tan(-pi/4.0_r_def + (i*inc)), i = 0, N) /)

incsteps = size(theta)
allocate(X(incsteps, incsteps), Y(incsteps, incsteps), Z(incsteps, incsteps))

Y = reshape( (/ theta /), (/ N+1, N+1 /), (/ theta /) )
X = transpose(Y)
Z = -1.0_r_def

allocate(S(N+1, N+1))
S = sqrt( 1.0_r_def / (X**2 + Y**2 + 1.0_r_def) )

X = X * S
Y = Y * S
Z = Z * S

deallocate(S)

allocate(Ex(N-1, 12), Ey(N-1, 12), Ez(N-1, 12))
Ex = 0.0_r_def; Ey = 0.0_r_def; Ez = 0.0_r_def

! Store the coordinates for the 4 Corner vertices on face 1
Vx(1) = X(1, 1);     Vy(1) = Y(1, 1);     Vz(1) = Z(1, 1);
Vx(2) = X(N+1, 1);   Vy(2) = Y(N+1, 1);   Vz(2) = Z(N+1, 1);
Vx(3) = X(1, N+1);   Vy(3) = Y(1, N+1);   Vz(3) = Z(1, N+1);
Vx(4) = X(N+1, N+1); Vy(4) = Y(N+1, N+1); Vz(4) = Z(N+1, N+1);

! Store the coordinates for the four edges on face 1
Ex(:,1) = X(2:N, 1);
Ey(:,1) = Y(2:N, 1);
Ez(:,1) = Z(2:N, 1);

Ex(:,2) = X(1, 2:N)
Ey(:,2) = Y(1, 2:N)
Ez(:,2) = Z(1, 2:N)

Ex(:,3) = X(N+1, 2:N)
Ey(:,3) = Y(N+1, 2:N)
Ez(:,3) = Z(N+1, 2:N)

Ex(:,4) = X(2:N, N+1)
Ey(:,4) = Y(2:N, N+1)
Ez(:,4) = Z(2:N, N+1)

allocate(Fx(N-1, N-1), Fy(N-1, N-1), Fz(N-1, N-1))
Fx = 0.0-r_def; Fy = 0.0_r_def; Fz = 0.0_r_def

! Store the internal vertices for face 1
Fx = X(2:N, 2:N)
Fy = Y(2:N, 2:N)
Fz = Z(2:N, 2:N)

Fstart = Estart + 12 * Elen
Fend = Fstart + Flen - 1

allocate(Vertices(6*N*N+2, 3))
Vertices = 0.0_r_def
call merge_vertices(Vertices(Fstart:Fend,:), Fx, Fy, Fz)

! Store the coordinates for the 4 vertices on face 2
Vx(5) = X(  1,   1); Vy(5) = Y(1,     1); Vz(5) = -Z(1,     1);
Vx(6) = X(N+1,   1); Vy(6) = Y(N+1,   1); Vz(6) = -Z(N+1,   1);
Vx(7) = X(  1, N+1); Vy(7) = Y(  1, N+1); Vz(7) = -Z(  1, N+1);
Vx(8) = X(N+1, N+1); Vy(8) = Y(N+1, N+1); Vz(8) = -Z(N+1, N+1);

! Move the 8 corners to final Vertices array
Vertices(1:8, :) = reshape((/ Vx, Vy, Vz /), (/ 8, 3 /))

! Store the coordinates for the four edges on face 2
Ex(:,5) =  X(2:N, 1)
Ey(:,5) =  Y(2:N, 1)
Ez(:,5) = -Z(2:N, 1)

Ex(:,6) =  X(1, 2:N)
Ey(:,6) =  Y(1, 2:N)
Ez(:,6) = -Z(1, 2:N)

Ex(:,7) =  X(N+1, 2:N)
Ey(:,7) =  Y(N+1, 2:N)
Ez(:,7) = -Z(N+1, 2:N)

Ex(:,8) =  X(2:N, N+1)
Ey(:,8) =  Y(2:N, N+1)
Ez(:,8) = -Z(2:N, N+1)

! Store the internal vertices for face 2
Fx(:,:) =  X(2:N,2:N)
Fy(:,:) =  Y(2:N,2:N)
Fz(:,:) = -Z(2:N,2:N)

! Move face 2 to final Vertices array
Fstart = Fstart + Flen
Fend   = Fend   + Flen

call merge_vertices(Vertices(Fstart:Fend,:), Fx, Fy, Fz)

! Calculate the coordinates for all of face 3, x = -1.0
! Face 3 = face 1 rotated about the y axis by pi/2: i.e (X, Y, Z) = (Z, Y, X)

! Store the coordinates for e9 and e10 on face 3
Ex(:, 9) = Z(1, 2:N)
Ey(:, 9) = Y(1, 2:N)
Ez(:, 9) = X(1, 2:N)

Ex(:,10) = Z(N+1, 2:N)
Ey(:,10) = Y(N+1, 2:N)
Ez(:,10) = X(N+1, 2:N)

! Store the internal vertices for face 3
Fx(:,:) = Z(2:N,2:N)
Fy(:,:) = Y(2:N,2:N)
Fz(:,:) = X(2:N,2:N)

! Move face 3 to final Vertices array
Fstart  = Fstart + Flen;
Fend    = Fend   + Flen;

call merge_vertices(Vertices(Fstart:Fend,:), Fx, Fy, Fz)
! Calculate the coordinates for all of face 4, x = 1.0
! Face 4 = face 1 rotated about the y axis by -pi/2: i.e (X, Y, Z) = (-Z, Y, X)

! Store the coordinates for e11 and e12 on face 4
Ex(:,11) = -Z(  1, 2:N)
Ey(:,11) =  Y(  1, 2:N)
Ez(:,11) =  X(  1, 2:N)

Ex(:,12) = -Z(N+1, 2:N)
Ey(:,12) =  Y(N+1, 2:N)
Ez(:,12) =  X(N+1, 2:N)

! Move the 12 edges to the final vertices array
call merge_vertices(Vertices(Estart: Estart + 12*Elen-1, :), Ex, Ey, Ez)

deallocate(Ex, Ey, Ez)

! Store the internal vertices for face 4
Fx(:,:) = -Z(2:N, 2:N);
Fy(:,:) =  Y(2:N, 2:N);
Fz(:,:) =  X(2:N, 2:N);

! Move face 4 to final Vertices array
Fstart  = Fstart + Flen;
Fend    = Fend   + Flen;

call merge_vertices(Vertices(Fstart:Fend,:), Fx, Fy, Fz)

! Calculate the coordinates for all of face 5, y = -1.0
! Face 5 = face 1 rotated about the x axis by -pi/2: i.e (X, Y, Z) = (X, Z, Y)

! Store the internal vertices for face 5
Fx(:,:) = X(2:N, 2:N);
Fy(:,:) = Z(2:N, 2:N);
Fz(:,:) = Y(2:N, 2:N);

! Move face 5 to final Vertices array
Fstart  = Fstart + Flen;
Fend    = Fend   + Flen;

call merge_vertices(Vertices(Fstart:Fend,:), Fx, Fy, Fz)

! Calculate the coordinates for all of face 6, y = 1.0
! Face 6 = face 1 rotated about the x axis by pi/2: i.e (X, Y, Z) = (X, -Z, Y)

! Store the internal vertices for face 6
Fx(:,:) = X(2:N,2:N); Fy(:,:) = -Z(2:N,2:N); Fz(:,:) = Y(2:N,2:N);

deallocate(X, Y, Z)

! Move face 6 to final Vertices array
Fstart  = Fstart + Flen;
Fend    = Fend   + Flen;

call merge_vertices(Vertices(Fstart:Fend,:), Fx, Fy, Fz)

deallocate(Fx, Fy, Fz)

if(.not.silent) then
    do i= 1, size(Vertices, 1)
        write(output_unit, "(3F11.6)") Vertices(i,:)
    end do
endif

deallocate(Vertices)

end subroutine write_vertices
!================================================
subroutine write_faces(N)
implicit none

! Calculate the quad -> edge connectivity and face ID
! Faces is a matrix with 6 * n * n rows, one row for each quad
! Each row contains for 4 integers representing the four vertices for the quad
! Each integer is a index into the vertex list, V
! The quads are ordered first by faces. Then in row, column order

integer (kind=i_def), intent(in)              :: N        ! divisions per edge

integer (kind=i_def)                          :: i, k
integer (kind=i_def)                          :: Fstart, Flen, Estart, fs, fe
integer (kind=i_def), dimension(:,:), pointer :: F        ! dim(N+1, N+1)
integer (kind=i_def), dimension(:,:), pointer :: Faces    ! dim(6*N*N, 4)

integer (kind=i_def), dimension(:,:), pointer :: e        ! dim(N-1, 12)
integer (kind=i_def), dimension(:,:), pointer :: f1       ! dim(N-1, N-1)
integer (kind=i_def), dimension(:,:), pointer :: V1, V2, V3, V4   ! dim(N,N)

Estart = 9
Flen   = (N-1)*(N-1)
Fstart = Estart + 12*(N-1)

allocate(e(N-1, 12), f1(N-1, N-1))
e = 0
f1 = 0

e = reshape( (/ (k, k=Estart, Fstart-1) /), (/ N-1, 12 /) )
f1 = reshape( (/ (k, k=Fstart, Fstart+Flen-1) /), (/ N-1, N-1 /))

! Calculate the indices for all the vertices on face 1
allocate(V1(N,N), V2(N,N), V3(N,N), V4(N,N))
allocate(F(N+1, N+1))
F = 0

F(1,:)   = reshape( (/ 1, e(:,2), 3 /), (/ N+1 /))
F(2:N,:) = reshape( (/ e(:,1), f1, e(:,4) /), (/ N-1, N+1 /))
F(N+1,:) = reshape( (/ 2, e(:,3), 4 /), (/ N+1 /))

V1 = F(1:ubound(F,1)-1, 1:ubound(F,1)-1)
V2 = F(2:ubound(F,1),   1:ubound(F,1)-1)
V3 = F(2:ubound(F,1),   2:ubound(F,1))
V4 = F(1:ubound(F,1)-1, 2:ubound(F,1))

fs = 1;
fe = N*N;

allocate(Faces(6*N*N, 4))
Faces = 0

call merge_faces(Faces(fs:fe, :), V1, V2, V3, V4)

! Calculate the indices for all the vertices on face 2

f1 = f1 + (N-1)**2

F(1,:) =   reshape( (/ 5, e(:,6), 7 /), (/ N+1 /))
F(2:N,:) = reshape( (/ e(:,5), f1, e(:,8) /), (/ N-1, N+1 /) )
F(N+1,:) = reshape( (/ 6, e(:, 7), 8 /), (/ N+1 /))

V1 = F(1:ubound(F,1)-1, 1:ubound(F,1)-1)
V2 = F(2:ubound(F,1),   1:ubound(F,1)-1)
V3 = F(2:ubound(F,1),   2:ubound(F,1))
V4 = F(1:ubound(F,1)-1, 2:ubound(F,1))

fs = fs + N*N;
fe = fe + N*N;

call merge_faces(Faces(fs:fe, :), V1, V2, V3, V4)

! Calculate the indices for all the vertices on face 3
f1 = f1 + (N-1)**2

F(1,:) =   reshape( (/ 1, e(:,9), 5 /), (/ N+1 /))
F(2:N,:) = reshape( (/  e(:,1), f1, e(:,5) /), (/ N-1, N+1 /) )
F(N+1,:) = reshape( (/  2, e(:,10), 6 /), (/ N+1 /))

V1 = F(1:ubound(F,1)-1, 1:ubound(F,1)-1)
V2 = F(2:ubound(F,1),   1:ubound(F,1)-1)
V3 = F(2:ubound(F,1),   2:ubound(F,1))
V4 = F(1:ubound(F,1)-1, 2:ubound(F,1))

fs = fs + N*N;
fe = fe + N*N;

call merge_faces(Faces(fs:fe, :), V1, V2, V3, V4)
! Calculate the indices for all the vertices on face 4
f1 = f1 + (N-1)**2

F(1,:) =   reshape( (/ 3, e(:,11), 7 /), (/ N+1 /))
F(2:N,:) = reshape( (/ e(:,4), f1, e(:,8) /), (/ N-1, N+1 /) )
F(N+1,:) = reshape( (/ 4, e(:,12), 8 /), (/ N+1 /))

V1 = F(1:ubound(F,1)-1, 1:ubound(F,1)-1)
V2 = F(2:ubound(F,1),   1:ubound(F,1)-1)
V3 = F(2:ubound(F,1),   2:ubound(F,1))
V4 = F(1:ubound(F,1)-1, 2:ubound(F,1))

fs = fs + N*N;
fe = fe + N*N;

call merge_faces(Faces(fs:fe, :), V1, V2, V3, V4)
! Calculate the indices for all the vertices on face 5
f1 = f1 + (N-1)**2

F(1,:) =   reshape( (/ 1, e(:,2), 3  /), (/ N+1 /))
F(2:N,:) = reshape( (/ e(:,9), f1, e(:,11) /), (/ N-1, N+1 /) )
F(N+1,:) = reshape( (/ 5, e(:,6), 7 /), (/ N+1 /))

V1 = F(1:ubound(F,1)-1, 1:ubound(F,1)-1)
V2 = F(2:ubound(F,1),   1:ubound(F,1)-1)
V3 = F(2:ubound(F,1),   2:ubound(F,1))
V4 = F(1:ubound(F,1)-1, 2:ubound(F,1))

fs = fs + N*N;
fe = fe + N*N;

call merge_faces(Faces(fs:fe, :), V1, V2, V3, V4)
! Calculate the indices for all the vertices on face 6

f1 = f1 + (N-1)**2

F(1,:) =   reshape( (/ 2, e(:,3), 4 /), (/ N+1 /))
F(2:N,:) = reshape( (/ e(:,10), f1, e(:,12) /), (/ N-1, N+1 /) )
F(N+1,:) = reshape( (/ 6, e(:,7), 8 /), (/ N+1 /))

V1 = F(1:ubound(F,1)-1, 1:ubound(F,1)-1)
V2 = F(2:ubound(F,1),   1:ubound(F,1)-1)
V3 = F(2:ubound(F,1),   2:ubound(F,1))
V4 = F(1:ubound(F,1)-1, 2:ubound(F,1))

deallocate(F)

fs = fs + N*N;
fe = fe + N*N;

call merge_faces(Faces(fs:fe, :), V1, V2, V3, V4)

deallocate(V1, V2, V3, V4)
deallocate(e, f1)

if(.not.silent) then
    do i=1,size(Faces,1)
        write(output_unit, "(4I12)") Faces(i,:)
    end do
endif

deallocate(Faces)

end subroutine write_faces
!================================================
end program cubedsphere
