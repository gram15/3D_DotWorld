' Giacomo Romoli 2018
' 3D world visualizer
'
'
'
'


' sub and functions
Declare Function projector_x(x_c as double, y_c as double, z_c as double, f_c as double) as double
Declare Function projector_y(x_c as double, y_c as double, z_c as double, f_c as double) as double
Declare Function rgb_col(r as UByte, g as UByte, b as UByte) as UInteger
Declare Function get_r(rgb_c as Uinteger) as UByte
Declare Function get_g(rgb_c as Uinteger) as UByte
Declare Function get_b(rgb_c as Uinteger) as UByte

Declare Sub pset3d(x as integer, y as integer, z as double, c as integer)
' it relies on global cam position and orientation
Declare Sub pix3d(x as double, y as double, z as double, c as integer)
Declare Sub redraw_all()
Declare Sub draw_user_pos(x_us as double, y_us as double, z_us as double, iHelp as integer)
Declare Sub setpixel(x as integer, y as integer, col as integer)
Declare Sub raycast(img as Ulong Ptr, bEnableRayTracing as boolean)

Type vector
   v1 as double
   v2 as double
   v3 as double
End Type

Type point3d
   x as double
   y as double
   z as double
End Type

Type tSphere
   xc as double
   yc as double
   zc as double
   radius as double
End Type

Type tPlane
   ' normal vector referred to plane origin
   x_normal as double
   y_normal as double
   z_normal as double
   ' extension u/v
   u_ext as double
   v_ext as double
End Type


' -----------------------------
'              MAIN
' -----------------------------


Dim shared as string sFileNameTxt
sFileNameTxt = "./green-grass-texture2.bmp"
Dim shared as Boolean bUseTexture, bUseFunction, bUseBigSphere, bUseSmallSpheres, bUsePoles
bUseTexture = FALSE
bUseFunction = FALSE
bUseBigSphere = FALSE
bUseSmallSpheres = FALSE
bUsePoles = FALSE



'             y+
'              |  z-
'              | /
'              |/
'              *-------x+
'             ICS

Dim shared As integer iDepth, iScreenWidth, iScreenHeight, iXc, iYc

'init
screen 17, 24 '(1024x768)
ScreenInfo iScreenWidth, iScreenHeight, iDepth,,,,
' center of the screen
iXc = iScreenWidth/2
iYc = iScreenHeight/2
print "Working on w: "; iScreenWidth; ", h: "; iScreenHeight; ", "; iDepth; " bit"

' init data
Dim myImage As ULong Ptr = ImageCreate(256, 256 )
Dim result as integer
result = BLoad(sFileNameTxt, myImage)
print "load result: "; result

Const PI As Double = 3.1415926535897932

dim shared as double focus,x_cam,y_cam,z_Cam,a1_cam,a2_cam,a3_cam
dim as integer iChar, iAskRedraw, iAskHelp, i, j, k, h, xv, yv
dim shared as double zBuffer(0 to iScreenWidth*iScreenHeight-1)

dim as integer xco, yco
dim as double radiangle

put (100,100), myImage
do:loop while asc(inkey$) <> 27

'init zBuffer
for i = 0 to iScreenWidth*iScreenHeight-1
    zBuffer(i) = -1000000
next i


' cam position
x_cam = -500
y_cam = 55
z_cam = 300

'x_cam = -500
'y_cam = 675
'z_cam = 526

' cam orientation
a1_cam = 0 'plane zx
a2_cam = 0 'plane yz
a3_cam = 0 'plane xy

'a1_cam = PI*45/180 'plane zx
'a2_cam = PI*55/180 'plane yz
'a3_cam = 0 'plane xy


' cam focus (850 on 1024)
focus = CInt(iScreenWidth * .83)

iAskRedraw = 1
iAskHelp = 0

'for i = 0 to 100
'    pset(512 + i, 384 - i), i + 255*250 + 255*255*250
'next i

'do: loop while INKEY$ <> chr$(27)
do
    if iAskRedraw = 1 then
        ' clear screen and init zBuffer
        cls        
        for i = 0 to iScreenWidth*iScreenHeight-1
            zBuffer(i) = -1000000
        next i

        raycast myImage, bUseTexture
        'raycast myImage, FALSE

        redraw_all

        draw_user_pos(x_cam, y_cam, z_cam, iAskHelp)
        'for radiangle = 0 to 359.99 step 1.0
        '   setpixel 50*cos(radiangle * PI / 180), 50*sin(radiangle * PI / 180), rgb_col(0, 255, 128)
        'next radiangle
        
        iAskRedraw = 0
    end if
    
    'for i = 0 to 100
    '    pset(512 + i, 384 - i), 250 + 255*1 + 255*255*1
    'next i

    ' key input management
    iChar = asc(inkey$)

    ' remove flicker 
    select case iChar
    case Asc("l")
        a1_cam = a1_cam + 3.1415/36
        iAskRedraw = 1
        
    case Asc("j")
        a1_cam = a1_cam - 3.1415/36       
        iAskRedraw = 1
        
    case Asc("k")
        a2_cam = a2_cam + 3.1415/36
        iAskRedraw = 1
        
    case Asc("i")
        a2_cam = a2_cam - 3.1415/36       
        iAskRedraw = 1
        
    case Asc("q")
        a3_cam = a3_cam - 3.1415/36       
        iAskRedraw = 1
        
    case Asc("e")
        a3_cam = a3_cam + 3.1415/36       
        iAskRedraw = 1
        
    case Asc("+")
        focus = focus - 25
        iAskRedraw = 1
        
    case Asc("-")
        focus = focus + 25
        iAskRedraw = 1        
        
    case Asc("w") 'forward
        x_cam = x_cam + 30 * cos(a2_cam) * sin(a1_cam)
        y_cam = y_cam - 30 * sin(a2_cam)
        z_cam = z_cam - 30 * cos(a2_cam) * cos(a1_cam)
        iAskRedraw = 1
        
    case Asc("s") 'backward
        x_cam = x_cam - 30 * cos(a2_cam) * sin(a1_cam)
        y_cam = y_cam + 30 * sin(a2_cam)
        z_cam = z_cam + 30 * cos(a2_cam) * cos(a1_cam)
        iAskRedraw = 1
        
    case Asc("h") 'help
        if iAskHelp = 0 then 
            iAskHelp = 1
        else
            iAskHelp = 0
        end if  
        iAskRedraw = 1      
        
    case Asc("x") ' printscreen
        bsave "printscreen.bmp", 0

    case Asc("t") 'use texture
        if bUseTexture = TRUE then
            bUseTexture = FALSE
        else
            bUseTexture = TRUE
        end if
        iAskRedraw = 1

    case Asc("1") 'use math function
        if bUseFunction = TRUE then
            bUseFunction = FALSE
        else
            bUseFunction = TRUE
        end if
        iAskRedraw = 1

    case Asc("2") 'use big sphere
        if bUseBigSphere = TRUE then
            bUseBigSphere = FALSE
        else
            bUseBigSphere = TRUE
        end if
        iAskRedraw = 1

    case Asc("3") 'use small sphere
        if bUseSmallSpheres = TRUE then
            bUseSmallSpheres = FALSE
        else
            bUseSmallSpheres = TRUE
        end if
        iAskRedraw = 1

    case Asc("4") 'use vert poles
        if bUsePoles = TRUE then
            bUsePoles = FALSE
        else
            bUsePoles = TRUE
        end if
        iAskRedraw = 1

    case else 
        iAskRedraw = 0
        
    end select
    

loop while iChar <> 27
'clean all
ImageDestroy (myImage)

'End of program

' --------------- FUNCTION DECLARATION -------------------------------

Function projector_x(x_c as double, y_c as double, z_c as double, f_c as double) as double
    'Dim dist as double
    'dist = abs(x_c)
    if z_c < 0 then
        projector_x = -(f_c) * x_c / z_c
    else
        projector_x = -2000
    end if
    
End Function

    
Function projector_y(x_c as double, y_c as double, z_c as double, f_c as double) as double
    'Dim dist as double
    'dist = abs(y_c)
    if z_c < 0 then
        projector_y = -(f_c) * y_c / z_c
    else
        projector_y = -2000
    end if
    
End Function

Function rgb_col(r as UByte, g as UByte, b as UByte) as UInteger
    rgb_col = b + 256*g + 256*256*r
End Function

Function get_r(rgb_c as Uinteger) as UByte
    get_r = (rgb_c AND &HFF0000) / (256*256)
End Function

Function get_g(rgb_c as Uinteger) as UByte
    get_g = (rgb_c AND &H00FF00) / (256)
End Function

Function get_b(rgb_c as Uinteger) as UByte
    get_b = (rgb_c AND &HFF)
End Function


Sub pset3d(x as integer, y as integer, z as double, c as integer)
    if x >= 0 AND x < iScreenWidth AND y >= 0 AND y < iScreenHeight AND z < 0 then
        if z > zBuffer(x + y*iScreenWidth) then
            pset(x,y),c
            'update zBuffer
            zBuffer(x + y*iScreenWidth) = z
        end if        
    end if    
End Sub

Sub pix3d(x as double, y as double, z as double, c as integer)
    Dim as double x_1, y_1, z_1, x_2, y_2, z_2, x_3, y_3, z_3, x_c, y_c, z_c, xv1, yv1
    Dim as integer xv,yv
    'translation
    x_c = x - x_cam
    y_c = y - y_cam
    z_c = z - z_cam
    
    ' rotation angle a1_cam zx
    x_1 = x_c * cos(a1_cam) + z_c * sin(a1_cam)
    y_1 = y_c
    z_1 = - x_c * sin(a1_cam) + z_c * cos(a1_cam)
    
    ' rotation angle a2_cam yz
    x_2 = x_1 
    y_2 = y_1 * cos(a2_cam) - z_1 * sin(a2_cam)
    z_2 = y_1 * sin(a2_cam) + z_1 * cos(a2_cam)
    
    ' rotation angle a3_cam xy
    x_3 = x_2 * cos(a3_cam) - y_2 * sin(a3_cam)
    y_3 = x_2 * sin(a3_cam) + y_2 * cos(a3_cam)
    z_3 = z_2
    
    xv = projector_x(x_3, y_3, z_3, focus)
    yv = projector_y(x_3, y_3, z_3, focus)
    
    'standard
    'pset3d(512 + xv, 384 - yv, z_3, c)
    
    'distorted
    'xv1 = 512*(sin(0.9*xv/512))
    'yv1 = 384*(sin(0.9*yv/384))
    'xv = xv1
    'yv = yv1
    
    pset3d(iXc + xv, iYc - yv, z_3, c)
    
    
End Sub

Sub DrawGrid()
    dim as double x,y,z, xstep, ystep, zstep, astep, bstep
    dim as integer i, j, k

    ' draw a grid
    for i = 0 to 100
        for xstep = 0 to 1000 step 1
            x = -500 + xstep
            z = -500 + 10 * i
            y = -50
            pix3d(x, y, z, rgb_col(128, 64, 128))
        next xstep
        for zstep = 0 to 1000 step 1
            x = -500 + 10 * i
            z = -500 + zstep
            y = -50
            pix3d(x, y, z, rgb_col(64, 128, 128))
        next zstep
    next i
End Sub

Sub DrawVerticalPoles()
    dim as double x,y,z, xstep, ystep, zstep, astep, bstep
    dim as integer i, j, k

    ' pali verticali
    for j = 0 to 10
        for ystep = 0 to 300 step 0.5
            x = -500 + 100 * j
            z = -500
            y = -50 + ystep
            pix3d(x, y, z, rgb_col(128, 128, 128))
        next ystep
    next j

    for j = 0 to 10
        for ystep = 0 to 50 step 0.5
            x = -500 + 600
            z = -500 + 100 * j
            y = -50 + ystep
            pix3d(x, y, z, rgb_col(128, 128, 128))
        next ystep
    next j

    for j = 0 to 10
        for ystep = 0 to 50 step 0.5
            x = -500 + 400
            z = -500 + 100 * j
            y = -50 + ystep
            pix3d(x, y, z, rgb_col(128, 128, 128))
        next ystep
    next j
End Sub

Sub DrawBigSphere()
    dim as double x,y,z, xstep, ystep, zstep, astep, bstep, radius
    dim as integer i, j, k

    'funzione sfera
    radius = 100
    for astep = -3.1415/2 to 3.1415*3/2 step 0.05
        for bstep = -3.1415/2 to 3.1415/2 step 0.05
            x = 400 + radius * cos(bstep) * cos(astep)
            z = -500 + radius * cos(bstep) * sin(astep)
            y = 50 + radius * sin(bstep)

            pix3d(x, y, z, rgb_col(255, 0, 100+y))
        next bstep
    next astep
End Sub

Sub DrawSmallSpheres()
    dim as double x,y,z, xstep, ystep, zstep, astep, bstep, radius
    dim as integer i, j, k

    ' small spheras
    radius = 5
    for k = 0 to 10
        for astep = -3.1415/2 to 3.1415*3/2 step 0.2
            for bstep = -3.1415/2 to 3.1415/2 step 0.2
                x = 100 + radius * cos(bstep) * cos(astep)
                z = -500 + k * 100 + radius * cos(bstep) * sin(astep)
                y = 0 + radius * sin(bstep)

                pix3d(x, y, z, rgb_col(255, 255, 0))

                x = - 100 + radius * cos(bstep) * cos(astep)
                z = -500 + k * 100 + radius * cos(bstep) * sin(astep)
                y = 0 + radius * sin(bstep)

                pix3d(x, y, z, rgb_col(255, 255, 0))

            next bstep
        next astep
    next k
End Sub

Sub DrawMathFunction1()
    dim as double x,y,z, xstep, ystep, zstep, astep, bstep, radius
    dim as integer i, j, k

    'mathematical function
    for astep = -200 to 200 step 2
        for bstep = -200 to 200 step 2
            x = 0 + astep
            z = 0 + bstep
            y = - 30 + 10*cos(astep/10)*cos(bstep/10)

            pix3d(x, y, z, rgb_col(0, 128+2*y, 128+y))
        next bstep
    next astep
End Sub

Sub DrawMathFunction2()
    dim as double x,y,z, xstep, ystep, zstep, astep, bstep, radius
    dim as integer i, j, k

    'y = e^-(x^2+z^2)
    for astep = -200 to 200 step 2
        for bstep = -200 to 200 step 2
            x = 0 + astep
            z = 0 + bstep
            y = - 30 + 40*1.0002^(-((astep)^2 + (bstep)^2))

            pix3d(x, y, z, rgb_col(0, 255, 128+2*y))
        next bstep
    next astep
End Sub

Sub redraw_all()
    dim as double x,y,z, xstep, ystep, zstep, astep, bstep
    'dim as integer iChar, iAskRedraw, i, j, k, h, xv, yv
    dim as integer i, j, k
    dim as boolean bEnableGrid
    
    bEnableGrid = false
    
    'griglia
    if bEnableGrid = true then
        DrawGrid
    end if
        
    ' pali verticali
    if bUsePoles = TRUE then
        DrawVerticalPoles
    end if
        
    'funzione sfera
    if bUseBigSphere = TRUE then
        DrawBigSphere
    end if
    
    ' small spheras
    if bUseSmallSpheres = TRUE then
        DrawSmallSpheres
    end if
    
    'mathematical function
    if bUseFunction = TRUE then
        DrawMathFunction1
    end if
    
    'y = e^-(x^2+z^2)
    if bUseFunction = TRUE then
        DrawMathFunction2
    end if
End Sub

' this sub draws a miniature of user position
Sub draw_user_pos(x_us as double, y_us as double, z_us as double, iHelp as integer)
    Dim as double x, y, z, x_circle, z_circle
    Dim as integer i, j, k, oriCol
    Dim as integer iUserWidth, iUserHeight, iStartX, iStartY
    iUserWidth = 100
    iUserHeight = 100
    iStartX = iScreenWidth - iUserWidth -1
    iStartY = iScreenHeight - iUserHeight -1    
    ' disegna rettangolo 100x100 in basso a dx (1024x768)

    ' rettangolo fondo nero
    'line (iStartX,iStartY)-(iStartX + iUserWidth, iStartY + iUserHeight), rgb_col(50,50,50), bf

    ' dithered transparence
    'for i = iStartX to (iStartX + iUserWidth - 1) step 2
    '    for j = iStartY to (iStartY + iUserHeight -1) step 2
    '        pset(i , j  ), rgb_col(50,50,50)
    '        pset(i+1,j+1), rgb_col(50,50,50)
    '    next j
    'next i

    ' transparence
    for i = iStartX to (iStartX + iUserWidth)
        for j = iStartY to (iStartY + iUserHeight)
            oriCol = point(i, j)
            pset(i, j), rgb_col(get_r(oriCol)/2, get_g(oriCol)/2, get_b(oriCol)/2)
        next j
    next i

    line (iStartX,iStartY)-(iStartX + iUserWidth, iStartY), rgb_col(250,250,250)
    line (iStartX,iStartY)-(iStartX, iStartY + iUserHeight), rgb_col(250,250,250)
    line (iStartX + iUserWidth,iStartY)-(iStartX + iUserWidth, iStartY + iUserHeight), rgb_col(250,250,250)
    line (iStartX,iStartY + iUserHeight)-(iStartX + iUserWidth, iStartY + iUserHeight), rgb_col(250,250,250)

    ' disegna la griglia principale 80x80 w0(-500,-500) -> R0(933,757)
    ' ratio: 100/8

    line (iStartX + 10,iStartY + 10)-(iStartX + 90, iStartY + 10), rgb_col(250,250,0)
    line (iStartX + 10,iStartY + 10)-(iStartX + 10, iStartY + 90), rgb_col(250,250,0)
    line (iStartX + 90,iStartY + 10)-(iStartX + 90, iStartY + 90), rgb_col(250,250,0)
    line (iStartX + 10,iStartY + 90)-(iStartX + 90, iStartY + 90), rgb_col(250,250,0)

    ' ratio conversion
    x = ((x_us + 500) * 8 / 100 + iStartX + 10)
    z = ((z_us - 500) * 8 / 100 + iStartY + 90)

    ' position
    circle (x, z), 3, rgb_col(250,0,0)

    ' orientation based on a1_cam
    x_circle = 8 * sin(a1_cam)
    z_circle = -8 * cos(a1_cam)

    line (x, z)-(x + x_circle, z + z_circle), rgb_col(250,250,0)
    

    ' other info
    locate 1,1

    print "3D DotWorld (GRom 2017) Press Esc to quit, h for help" 

    if iHelp = 1 then
        print "Info key: forward (w), backward (s), left (j), right (l), up (i), down (k), tilt left (q), tilt right (e), focus adjust (+)(-)"
    else
        print " "
    end if

    print "x = "; cint(x_us); "   y = "; cint(y_us); "   z = "; cint(z_us); "     "
    print "a1 = "; cint(a1_cam*180/3.1415); "   a2 = "; cint(a2_cam*180/3.1415); "   a3 = "; cint(a3_cam*180/3.1415); "     "
    print "focus = "; focus; "    " 

End Sub

' put a pixel in the viewport with transformation of coordinates
Sub setpixel(x as integer, y as integer, col as integer)
    ' Note
    ' Screen: 
    '  o---------> xV             ^ yS
    '  |                          |
    '  |              =>          |
    '  |                   -------o--------> xS   
    '  v                          |
    '  yV                         |

    'dim as integer iXc, iYc
    ' screen center
    'iXc = 1024/2
    'iYc = 768/2
    'iXc = iScreenWidth/2
    'iYc = iScreenHeight/2
   
    pset(x + iXc, iYc - y), col
   
End Sub


Sub raycast(img as ULong Ptr, bEnableRayTracing as boolean)
    ' Notes: this function doesn't care about the geometry rototranslation accorded to an_cam angles

    'dim as integer ic, jc, iX, iY, iXc, iYc, iCollision
    dim as integer ic, jc, iX, iY, iCollision
    dim as double dX, dY, dZ, dcX, dcY, dcZ, dT, vm, t, px, py, pz, vv1, vv2, vv3, nDotA, nDotBA, dpx, dpy, dpz
    dim as Uinteger p_color, pc1, pc2, pc3, pc4
    dim as vector vrad, vrad_n, vrad_n_temp, v_plane_normal
    dim as point3d cam_origin, ray_origin, plane_origin, plane_collision
   
    ' center of the screen
    'iXc = 1024/2
    'iYc = 768/2
    'iXc = iScreenWidth/2
    'iYc = iScreenHeight/2


   ' set the value of tiling
#define iTextureModule 256
   
   print iXc, iYc
   'do: loop while inkey$=""
   
   ' P1 ray origin
   ray_origin.x = x_cam
   ray_origin.y = y_cam
   ray_origin.z = z_cam
      
   dcX = x_cam
   dcY = y_cam
   dcZ = z_cam
   
   'Plane origin d
   plane_origin.x = 0
   plane_origin.y = -50
   plane_origin.z = 0
   
   px = 0
   py = 0
   pz = 0
   
   'Plane normal n
   v_plane_normal.v1 = 0
   v_plane_normal.v2 = 1
   v_plane_normal.v3 = 0
   
   
   ' P1 ray origin
   cam_origin.x = x_cam - plane_origin.x
   cam_origin.y = y_cam - plane_origin.y
   cam_origin.z = z_cam - plane_origin.z
   
   locate 1,1
   print "x_cam = "; cint(cam_origin.x) ; "  y_cam = "; cint(cam_origin.y) ; "  z_cam = "; cint(cam_origin.z) ; 
   print
   
   ' screen scan from -1024/2 to 1024/2
   for ic = 0 to iScreenWidth 
      for jc = 0 to iScreenHeight
        
         ' coordinate trasformation (VWorld ref to cam ref)
         dX = ic - iXc
         dY = iYc - jc
         dZ = focus
                   
         ' versore raggio proiettato
         'vm = ((dX - cam_origin.x)^2 + (dY - cam_origin.y)^2 + (dZ - cam_origin.z)^2)^.5
         'vrad_n.v1 = (dX - cam_origin.x)/vm
         'vrad_n.v2 = (dY - cam_origin.y)/vm
         'vrad_n.v3 = (dZ - cam_origin.z)/vm
         vm = (dX^2 + dY^2 + dZ^2)^.5
         vrad_n.v1 = dX/vm 
         vrad_n.v2 = dY/vm
         vrad_n.v3 = dZ/vm
         
         'rotation a3
         vrad_n_temp.v1 = vrad_n.v1 * cos(-a3_cam) - vrad_n.v2 * sin(-a3_cam) 
         vrad_n_temp.v2 = vrad_n.v1 * sin(-a3_cam) + vrad_n.v2 * cos(-a3_cam)
         vrad_n_temp.v3 = vrad_n.v3
         
         vrad_n.v1 = vrad_n_temp.v1
         vrad_n.v2 = vrad_n_temp.v2
         vrad_n.v3 = vrad_n_temp.v3

         ' rotation a2
         vrad_n_temp.v1 = vrad_n.v1 * 1 + vrad_n.v2 * 0 + vrad_n.v3 * 0 
         vrad_n_temp.v2 = vrad_n.v2 * cos(a2_cam) - vrad_n.v3 * sin(a2_cam)
         vrad_n_temp.v3 = vrad_n.v2 * sin(a2_cam) + vrad_n.v3 * cos(a2_cam) 
         
         vrad_n.v1 = vrad_n_temp.v1
         vrad_n.v2 = vrad_n_temp.v2
         vrad_n.v3 = vrad_n_temp.v3
      
         'rotation a1
         vrad_n_temp.v1 = vrad_n.v1 * cos(a1_cam) + vrad_n.v3 * sin(a1_cam) 
         vrad_n_temp.v2 = vrad_n.v1 * 0 + vrad_n.v2 * 1 + vrad_n.v3 * 0
         vrad_n_temp.v3 = - vrad_n.v1 * sin(a1_cam) + vrad_n.v3 * cos(a1_cam) 
         
         vrad_n.v1 = vrad_n_temp.v1
         vrad_n.v2 = vrad_n_temp.v2
         vrad_n.v3 = vrad_n_temp.v3

         
         if dX = 0 AND dY = 0 then
            setpixel dX, dY, rgb_col(0, 255, 255)
            setpixel dX, dY, rgb_col(vm/2000, vm/2000, vm/2000)
            print vm
         end if
         'p_color=vm/3
         'print vm, p_color
         'setpixel dX, dY, rgb_col(p_color, p_color, p_color)
         
         
         ' ray
         'Vector3 Plane::intersectLine(Vector3 a, Vector3 b) {
         '    Vector3 ba = b-a;           
         vrad.v1 = dX' - cam_origin.x
         vrad.v2 = dY' - cam_origin.y
         vrad.v3 = dZ' - cam_origin.z
         
         
         ' plane rendering
            '    float nDotA = Vector3::dotProduct(n, a);
         nDotA = v_plane_normal.v1*cam_origin.x + v_plane_normal.v2*cam_origin.y + v_plane_normal.v3*cam_origin.z
         
            '    float nDotBA = Vector3::dotProduct(n, ba);
         nDotBA = v_plane_normal.v1*vrad_n.v1 + v_plane_normal.v2*vrad_n.v2 + v_plane_normal.v3*vrad_n.v3
         
         'if ic = 50 AND jc = 50 then
         '   print nDotA
         '   print nDotBA
         'end if
         
            ' singularity
         if nDotBA < 0 then
            '    return a + (((d - nDotA)/nDotBA) * ba);
            'collision point
            plane_collision.x = cam_origin.x + ((0 - nDotA)/nDotBA) * vrad_n.v1
            plane_collision.y = cam_origin.y - ((0 - nDotA)/nDotBA) * vrad_n.v2
            plane_collision.z = cam_origin.z - ((0 - nDotA)/nDotBA) * vrad_n.v3
               'x_1 = x_c * cos(a1_cam) + z_c * sin(a1_cam)
               'y_1 = y_c
               'z_1 = - x_c * sin(a1_cam) + z_c * cos(a1_cam)
            
            px = plane_collision.x
            py = plane_collision.y
            pz = plane_collision.z
            
            'rotation
            'px = ( plane_collision.x * cos(a1_cam) + plane_collision.z * sin(a1_cam))
            'py = ( plane_collision.y)
            'pz = ( (-plane_collision.x) *  sin(a1_cam) + plane_collision.z * cos(a1_cam))
             
            'plane_collision.x = px
            'plane_collision.y = py
            'plane_collision.z = px 
             
            'px = ( plane_collision.x )
            'py = ( plane_collision.y * cos(a2_cam) - plane_collision.z * sin(a2_cam) )
            'pz = ( plane_collision.y * sin(a2_cam) + plane_collision.z * cos(a2_cam) )
             
            'if (py > cam_origin.y) AND nDotBA < 0 then
            ' Draw if toward the cam
            if (py > cam_origin.y) then
                
                if bEnableRayTracing = TRUE then
                    ' Color assignment
                    if (px > -500 AND px < 500 AND pz > -500 AND pz < 500) then
                        if ((((px+50) /10.0) MOD 2 = 0) XOR (((pz+50)/10.0) MOD 2 = 0)) then
                            p_color = rgb_col(0, 255*abs(1.0-nDotBA), 128*abs(1.0-nDotBA))
                        else
                            p_color = rgb_col(0, 0, 128*abs(nDotBA))
                        end if
                      
                        'pset(dX + iXc, dY + iYc), p_color                    
                        setpixel dX, dY, p_color                    
                    end if
                else
                    if (px > -500 AND px < 500 AND pz > -500 AND pz < 500) then
                    'if (px > 0 AND px < 279 AND pz > 0 AND pz < 279) then
                        'setpixel dX, dY, rgb_col(128,128,128)
                        'p_color = img[(((px+500) MOD 256) + 256*((pz+500) MOD 256))]                        
                        p_color = img[(((px+500) MOD iTextureModule) + iTextureModule*((pz+500) MOD iTextureModule))]  
                        'dim idx as integer
                        'idx = (((px+500) MOD iTextureModule) + iTextureModule*((pz-1+500) MOD iTextureModule))
                        'if (idx < 0 ) then
                            'print "Negative index idx: "; idx; ", px =  "; px; ", pz = "; pz
                        'else
                            'pc1 = img[CInt(((px+500) MOD iTextureModule) + iTextureModule*((pz + iTextureModule -1+500) MOD iTextureModule))]
                            'pc2 = img[CInt(((px+1+500) MOD iTextureModule) + iTextureModule*((pz+500) MOD iTextureModule))]
                            'pc3 = img[CInt(((px+500) MOD iTextureModule) + iTextureModule*((pz+1+500) MOD iTextureModule))]
                            'pc4 = img[CInt(((px-1+500) MOD iTextureModule) + iTextureModule*((pz+500) MOD iTextureModule))]
                            pc1 = img[CInt(((px+500) MOD iTextureModule) + ((pz + iTextureModule -1+500) MOD iTextureModule) Shl 8)]
                            pc2 = img[CInt(((px+1+500) MOD iTextureModule) + ((pz+500) MOD iTextureModule) Shl 8)]
                            pc3 = img[CInt(((px+500) MOD iTextureModule) + ((pz+1+500) MOD iTextureModule) Shl 8)]
                            pc4 = img[CInt(((px-1+500) MOD iTextureModule) + ((pz+500) MOD iTextureModule) Shl 8)]

                            'setpixel dX, dY, p_color                    
                            setpixel dX, dY, _
                                rgb_col((get_r(p_color) + get_r(pc1) + get_r(pc2) + get_r(pc3) + get_r(pc4))/5, _
                                (get_g(p_color) + get_g(pc1) + get_g(pc2) + get_g(pc3) + get_g(pc4))/5, _
                                (get_b(p_color) + get_b(pc1) + get_b(pc2) + get_b(pc3) + get_b(pc4))/5 _
                            )
                        'end if                        
                    end if
                end if
                 
            end if
             
             
             '}
         end if
      
         
      next jc
   next ic
End Sub
