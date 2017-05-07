' 3D DotWorld Engine, by GRom 2017 jamiroval@gmail.com

' sub and functions
Declare Function projector_x(x_c as double, y_c as double, z_c as double, f_c as double) as double
Declare Function projector_y(x_c as double, y_c as double, z_c as double, f_c as double) as double
Declare Function rgb_col(r as UByte, g as UByte, b as UByte) as integer
Declare Sub pset3d(x as integer, y as integer, z as double, c as integer)
' it relies on global cam position and orientation
Declare Sub pix3d(x as double, y as double, z as double, c as integer)
Declare Sub draw_scene()
Declare Sub draw_user_pos(x_us as double, y_us as double, z_us as double, iHelp as integer)


'             y+
'              |  z-
'              | /
'              |/
'              *-------x+
'             ICS


screen 20, 24

dim shared as double focus, fade_dist, x_cam,y_cam,z_Cam,a1_cam,a2_cam,a3_cam
dim as integer iChar, iAskRedraw, iAskHelp, i, j, k, h, xv, yv
dim shared as double zBuffer(0 to 1024*768-1)
'init zBuffer
for i = 0 to 1024*768-1
    zBuffer(i) = -1000000
next i


' cam position
x_cam = -500
y_cam = 55
z_cam = 300

' cam orientation
a1_cam = 0 'plane zx
a2_cam = 0 'plane yz
a3_cam = 0 'plane xy

' cam focus
focus = 850

fade_dist = 2000

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
        for i = 0 to 1024*768-1
            zBuffer(i) = -1000000
        next i

		draw_scene()
		draw_user_pos(x_cam, y_cam, z_cam, iAskHelp)
    
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
                
    case else 
        iAskRedraw = 0
        
    end select
    

loop while iChar <> 27


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

Function rgb_col(r as UByte, g as UByte, b as UByte) as integer
    rgb_col = b + 256*g + 256*256*r
End Function

Sub pset3d(x as integer, y as integer, z as double, c as integer)
    if x >= 0 AND x < 1024 AND y >= 0 AND y < 768 AND z < 0 then
        if z > zBuffer(x + y*1024) then
            pset(x,y),c
            'update zBuffer
            zBuffer(x + y*1024) = z
        end if        
    end if    
End Sub

Sub pix3d(x as double, y as double, z as double, c as integer)
    Dim as double x_1, y_1, z_1, x_2, y_2, z_2, x_3, y_3, z_3, x_c, y_c, z_c, xv1, yv1
    Dim as integer xv,yv
    Dim as integer r, g, b
    
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
    
    ' color modification - fade to black    
    r = ((c - (c MOD (256*256)))/ (256*256))
    g = (((c MOD (256*256)) - (c MOD 256))/ 256)
    b = (c MOD 256)
    
    if z_3 < -fade_dist OR z_3 >= 0 then
		r = 0
		g = 0
		b = 0
	else
		r = r * ((fade_dist+z_3) / fade_dist)
		g = g * ((fade_dist+z_3) / fade_dist)
		b = b * ((fade_dist+z_3) / fade_dist)
				
	end if
    
    
    'pset3d(512 + xv, 384 - yv, z_3, c)
    pset3d(512 + xv, 384 - yv, z_3, rgb_col(r, g, b))
    
    
End Sub


' sub which draws all the object on the scene
Sub draw_scene()

	Dim as double x,y,z, xstep, ystep, zstep, astep, bstep
	Dim as integer i, j, k
	
	'griglia 1000x1000 
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
		
	'funzione sfera
	dim radius as double
	radius = 100
	for astep = -3.1415/2 to 3.1415*3/2 step 0.05
		for bstep = -3.1415/2 to 3.1415/2 step 0.05
			x = 400 + radius * cos(bstep) * cos(astep)
			z = -500 + radius * cos(bstep) * sin(astep)
			y = 50 + radius * sin(bstep)
			
			pix3d(x, y, z, rgb_col(255, 0, 100+y))
		next bstep
	next astep
	
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
	
	'mathematical function
	for astep = -200 to 200 step 1
		for bstep = -200 to 200 step 2
			x = 0 + astep
			z = 0 + bstep
			y = - 30 + 10*cos(astep/10)*cos(bstep/10)
			
			pix3d(x, y, z, rgb_col(0, 128+2*y, 128+y))
		next bstep
	next astep
	
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

' this sub draws a miniature of user position
Sub draw_user_pos(x_us as double, y_us as double, z_us as double, iHelp as integer)
	Dim as double x, y, z, x_circle, z_circle
	Dim as integer i, j, k
	
	' disegna rettangolo 100x100 in basso a dx (1024x768)
	' rettangolo fondo nero
	line (923,667)-(1023, 767), rgb_col(50,50,50), bf
	
	
	line (923,667)-(1023, 667), rgb_col(250,250,250)
	line (923,667)-(923, 767), rgb_col(250,250,250)
	line (1023,667)-(1023, 767), rgb_col(250,250,250)
	line (923,767)-(1023, 767), rgb_col(250,250,250)
	
	' disegna la griglia principale 80x80 w0(-500,-500) -> R0(933,757)
	' ratio: 100/8
	
	line (933,677)-(1013, 677), rgb_col(250,250,0)
	line (933,677)-(933, 757), rgb_col(250,250,0)
	line (1013,677)-(1013, 757), rgb_col(250,250,0)
	line (933,757)-(1013, 757), rgb_col(250,250,0)
	
	' ratio conversion
	x = ((x_us + 500) * 8 / 100 + 933)
	z = -((z_us + 500) * 8 / 100 - 757)
	
	' position
	circle (x, z), 3, rgb_col(250,0,0)
	
	' orientation based on a1_cam
	x_circle = 8 * sin(a1_cam)
	z_circle = 8 * cos(a1_cam)
	
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
