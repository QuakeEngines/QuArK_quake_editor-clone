-------------------------------------
--MD2 Exporter
--Version 1.01 for max 4
--Adam Barton 
--Email adam@add3d.co.uk
-------------------------------------

global md2lastfile=""
global MD2Export
global nlist=#([-0.525731, 0.000000, 0.850651],[-0.442863, 0.238856, 0.864188],[-0.295242, 0.000000, 0.955423],[-0.309017, 0.500000, 0.809017],[-0.162460, 0.262866, 0.951056],[0.000000, 0.000000, 1.000000],[0.000000, 0.850651, 0.525731],[-0.147621, 0.716567, 0.681718],[0.147621, 0.716567, 0.681718],[0.000000, 0.525731, 0.850651],[0.309017, 0.500000, 0.809017],[0.525731, 0.000000, 0.850651],[0.295242, 0.000000, 0.955423],[0.442863, 0.238856, 0.864188],[0.162460, 0.262866, 0.951056],[-0.681718, 0.147621, 0.716567],[-0.809017, 0.309017, 0.500000],[-0.587785, 0.425325, 0.688191],[-0.850651, 0.525731, 0.000000],[-0.864188, 0.442863, 0.238856],[-0.716567, 0.681718, 0.147621],[-0.688191, 0.587785, 0.425325],[-0.500000, 0.809017, 0.309017],[-0.238856, 0.864188, 0.442863],[-0.425325, 0.688191, 0.587785],[-0.716567, 0.681718, -0.147621],[-0.500000, 0.809017, -0.309017],[-0.525731, 0.850651, 0.000000],[0.000000, 0.850651, -0.525731],[-0.238856, 0.864188, -0.442863],[0.000000, 0.955423, -0.295242],[-0.262866, 0.951056, -0.162460],[0.000000, 1.000000, 0.000000],[0.000000, 0.955423, 0.295242],[-0.262866, 0.951056, 0.162460],[0.238856, 0.864188, 0.442863],[0.262866, 0.951056, 0.162460],[0.500000, 0.809017, 0.309017],[0.238856, 0.864188, -0.442863],[0.262866, 0.951056, -0.162460],[0.500000, 0.809017, -0.309017],[0.850651, 0.525731, 0.000000],[0.716567, 0.681718, 0.147621],[0.716567, 0.681718, -0.147621],[0.525731, 0.850651, 0.000000],[0.425325, 0.688191, 0.587785],[0.864188, 0.442863, 0.238856],[0.688191, 0.587785, 0.425325],[0.809017, 0.309017, 0.500000],[0.681718, 0.147621, 0.716567],[0.587785, 0.425325, 0.688191],[0.955423, 0.295242, 0.000000],[1.000000, 0.000000, 0.000000],[0.951056, 0.162460, 0.262866],[0.850651, -0.525731, 0.000000],[0.955423, -0.295242, 0.000000],[0.864188, -0.442863, 0.238856],[0.951056, -0.162460, 0.262866],[0.809017, -0.309017, 0.500000],[0.681718, -0.147621, 0.716567],[0.850651, 0.000000, 0.525731],[0.864188, 0.442863, -0.238856],[0.809017, 0.309017, -0.500000],[0.951056, 0.162460, -0.262866],[0.525731, 0.000000, -0.850651],[0.681718, 0.147621, -0.716567],[0.681718, -0.147621, -0.716567],[0.850651, 0.000000, -0.525731],[0.809017, -0.309017, -0.500000],[0.864188, -0.442863, -0.238856],[0.951056, -0.162460, -0.262866],[0.147621, 0.716567, -0.681718],[0.309017, 0.500000, -0.809017],[0.425325, 0.688191, -0.587785],[0.442863, 0.238856, -0.864188],[0.587785, 0.425325, -0.688191],[0.688191, 0.587785, -0.425325],[-0.147621, 0.716567, -0.681718],[-0.309017, 0.500000, -0.809017],[0.000000, 0.525731, -0.850651],[-0.525731, 0.000000, -0.850651],[-0.442863, 0.238856, -0.864188],[-0.295242, 0.000000, -0.955423],[-0.162460, 0.262866, -0.951056],[0.000000, 0.000000, -1.000000],[0.295242, 0.000000, -0.955423],[0.162460, 0.262866, -0.951056],[-0.442863, -0.238856, -0.864188],[-0.309017, -0.500000, -0.809017],[-0.162460, -0.262866, -0.951056],[0.000000, -0.850651, -0.525731],[-0.147621, -0.716567, -0.681718],[0.147621, -0.716567, -0.681718],[0.000000, -0.525731, -0.850651],[0.309017, -0.500000, -0.809017],[0.442863, -0.238856, -0.864188],[0.162460, -0.262866, -0.951056],[0.238856, -0.864188, -0.442863],[0.500000, -0.809017, -0.309017],[0.425325, -0.688191, -0.587785],[0.716567, -0.681718, -0.147621],[0.688191, -0.587785, -0.425325],[0.587785, -0.425325, -0.688191],[0.000000, -0.955423, -0.295242],[0.000000, -1.000000, 0.000000],[0.262866, -0.951056, -0.162460],[0.000000, -0.850651, 0.525731],[0.000000, -0.955423, 0.295242],[0.238856, -0.864188, 0.442863],[0.262866, -0.951056, 0.162460],[0.500000, -0.809017, 0.309017],[0.716567, -0.681718, 0.147621],[0.525731, -0.850651, 0.000000],[-0.238856, -0.864188, -0.442863],[-0.500000, -0.809017, -0.309017],[-0.262866, -0.951056, -0.162460],[-0.850651, -0.525731, 0.000000],[-0.716567, -0.681718, -0.147621],[-0.716567, -0.681718, 0.147621],[-0.525731, -0.850651, 0.000000],[-0.500000, -0.809017, 0.309017],[-0.238856, -0.864188, 0.442863],[-0.262866, -0.951056, 0.162460],[-0.864188, -0.442863, 0.238856],[-0.809017, -0.309017, 0.500000],[-0.688191, -0.587785, 0.425325],[-0.681718, -0.147621, 0.716567],[-0.442863, -0.238856, 0.864188],[-0.587785, -0.425325, 0.688191],[-0.309017, -0.500000, 0.809017],[-0.147621, -0.716567, 0.681718],[-0.425325, -0.688191, 0.587785],[-0.162460, -0.262866, 0.951056],[0.442863, -0.238856, 0.864188],[0.162460, -0.262866, 0.951056],[0.309017, -0.500000, 0.809017],[0.147621, -0.716567, 0.681718],[0.000000, -0.525731, 0.850651],[0.425325, -0.688191, 0.587785],[0.587785, -0.425325, 0.688191],[0.688191, -0.587785, 0.425325],[-0.955423, 0.295242, 0.000000],[-0.951056, 0.162460, 0.262866],[-1.000000, 0.000000, 0.000000],[-0.850651, 0.000000, 0.525731],[-0.955423, -0.295242, 0.000000],[-0.951056, -0.162460, 0.262866],[-0.864188, 0.442863, -0.238856],[-0.951056, 0.162460, -0.262866],[-0.809017, 0.309017, -0.500000],[-0.864188, -0.442863, -0.238856],[-0.951056, -0.162460, -0.262866],[-0.809017, -0.309017, -0.500000],[-0.681718, 0.147621, -0.716567],[-0.681718, -0.147621, -0.716567],[-0.850651, 0.000000, -0.525731],[-0.688191, 0.587785, -0.425325],[-0.587785, 0.425325, -0.688191],[-0.425325, 0.688191, -0.587785],[-0.425325, -0.688191, -0.587785],[-0.587785, -0.425325, -0.688191],[-0.688191, -0.587785, -0.425325])

fn addcheck cb=
(
if classof cb==string do messagebox cb title:"MD2 Export Error"
)


struct MD2_Tex
(
	s,t
)
	

struct MD2_Vertex
(
	pos, lightNormalIndex
)

struct MD2_Frame
(
	scale, translate, name,
	vList,vNorm	
)

struct MD2_Tri
(
	v1,v2,v3,				--vertIndices
	t1,t2,t3				--texCoordIndices
)

struct MD2_model
(
	skinlist,
	texlist,
	trilist,
	framelist
)

fn makemd2 object=
(
if md2export.useanim.state==1 then 
	(
	astart=animationrange.start
	aend=animationrange.end
	)
else
	(
	astart=md2export.animstart.value
	aend=md2export.animend.value
	)
if not(md2export.oneframe.state) do 
	(
	astart=1
	aend=1
	)
	
obj=(object)
md2=md2_model skinlist:#("SkinBitMap") texlist:#() trilist:#() framelist:#()

------------------------------------
-- build texture vert list
------------------------------------
for i=1 to getnumtverts obj do
	(
	s=(((getTVert obj i).x)*256) as integer
	t=((-(getTVert obj i).y+1)*256) as integer
	append md2.texlist (md2_tex s:s t:t)
	)
------------------------------------
-- build triangle list
------------------------------------
for i=1 to obj.numfaces do
	(
	v1=((getFace obj i).z)-1
	v2=((getFace obj i).y)-1
	v3=((getFace obj i).x)-1
	t1=((getTVFace obj i).z)-1
	t2=((getTVface obj i).y)-1
	t3=((getTVface obj i).x)-1
	append md2.trilist (md2_Tri v1 v2 v3 t1 t2 t3)
	)
-------------------------------------
-- build frame list
-------------------------------------
--print astart
--print aend
for f=astart to aend by md2export.stepframe.value do 
	(
	local vertlist=#()
	local normlist=#()
	frame=MD2_frame scale:[1,1,1] translate:[0,0,0] name:"FRAME 000......"       
	vmin=[0,0,0]
	vmax=[0,0,0]
	for i=1 to obj.numverts do at time f
		(
		local v=in coordsys world(getvert obj i)
		--v+=object.pos
		--v*=object.scale
		--v-=object.pos
		if v.x<vmin.x do vmin.x=v.x
		if v.y<vmin.y do vmin.y=v.y
		if v.z<vmin.z do vmin.z=v.z
		if v.x>vmax.x do vmax.x=v.x
		if v.y>vmax.y do vmax.y=v.y
		if v.z>vmax.z do vmax.z=v.z	
		)
	frame.translate=vmin
	frame.scale=(vmax-vmin)/[255,255,255]
	
	for i=1 to obj.numverts do at time f
		(
		local v=in coordsys world (getvert obj i)
		--v+=object.pos
		--v*=object.scale
		--v-=object.pos
		v-=frame.translate
		v/=frame.scale
		append vertlist v
		------------------------------------
		--do normals
		------------------------------------
		local vn=in coordsys world (getnormal obj i)
		local maxdot=-99999.0
		local normindex=1
		if md2export.donormals.state do
		for j=1 to nlist.count do
			(
			local dotvalue
			dotvalue=dot nlist[j] vn
			if dotvalue>maxdot do
				(
				maxdot=dotvalue
				normindex=j
				)
			)
		append normlist normindex 
		)
	frame.vList=vertlist
	frame.vNorm=normlist
	append md2.framelist frame
	)	
md2
)



fn writemd2 obj md2file=
(
	if obj==undefined do return"No object"
	if (classof obj)!=editable_mesh do return "Object is not a mesh"
	model=makemd2 obj
	
	f=fopen md2file "wb"
	
	----------------------------------------
	-- write the header
	----------------------------------------
	
	magic=844121161
	version=8
	skinw=256
	skinh=256
	framenum=model.framelist.count
	skinnum=model.skinlist.count
	vertnum=model.framelist[1].vlist.count
	texnum=model.texlist.count
	trinum=model.trilist.count
	glnum=0
	
	texsize=4
	trisize=12
	framesize=40+(4*vertnum)
	
	headersize=68				-- bytes in header
	offsetskins=headersize
	offsettexcoords=offsetskins+(skinnum*64)
	offsettris=offsettexcoords+(texnum*texsize)
	offsetframes=offsettris+(trinum*trisize)
	offsetgl=offsetframes+(framenum*framesize)
	offsetend=offsetgl+(glnum*4)
	
	writelong f magic
	writelong f version
	writelong f skinw				--(blitz dont care)
	writelong f skinh				--(blitz dont care)
	writelong f framesize
	writelong f skinnum
	writelong f vertnum
	writelong f texnum
	writelong f trinum
	writelong f glnum
	writelong f framenum
	writelong f offsetskins		--offset to skins
	writelong f offsettexcoords	--offset to texture coords
	writelong f offsettris		--offset to triangles
	writelong f offsetframes		--offset to frames
	writelong f offsetgl			--offset to gl commands
	writelong f offsetend			--offset to end

-----------------------------------------------------
-- end of header
-----------------------------------------------------
-- write out the skin names
-----------------------------------------------------
	for i in model.skinlist do
	(
		skinname=i
		while skinname.count<63 do skinname+=":"
		writestring f skinname
	)

-----------------------------------------------------
-- write texture coordinate list
-----------------------------------------------------
	for i in model.texlist do
	(
		writeshort f i.s #unsigned
		writeshort f i.t #unsigned
	)
	
-----------------------------------------------------
--write triangles
-----------------------------------------------------	
	for i in model.trilist do
	(
		writeshort f i.v1 #unsigned
		writeshort f i.v2 #unsigned
		writeshort f i.v3 #unsigned
		writeshort f i.t1 #unsigned
		writeshort f i.t2 #unsigned
		writeshort f i.t3 #unsigned
	)

-----------------------------------------------------
-- write frames
-----------------------------------------------------
	for i in model.framelist do
	(
		writefloat f i.scale.x
		writefloat f i.scale.y
		writefloat f i.scale.z
		writefloat f i.translate.x
		writefloat f i.translate.y
		writefloat f i.translate.z
		writestring f i.name
		k=0
		for j=1 to i.vlist.count do
		(
			writebyte f i.vlist[j].x
			writebyte f i.vlist[j].y
			writebyte f i.vlist[j].z
			writebyte f i.vNorm[j]
		)
	)
------------------------------------------------------
-- end of file writing
------------------------------------------------------
fclose f
)


utility MD2Export "MD2 Exporter"
(
	group "Export"
	(
		checkbox oneframe "Save Animation" checked:false align:#center
		spinner stepframe "Frame Step" range:[1,200,1] fieldwidth:40 type:#integer
		radiobuttons useanim labels:#("Active Time Segment","Custom Time Segment")
		spinner animstart range:[0,1000,0] type:#integer fieldwidth:40 across:2
		spinner animend "to" range:[0,1000,40] type:#integer fieldwidth:40
		checkbox donormals "Generate Normals" align:#center checked:false
		button bexport "     Export    "
	)
	group "About"
	(
		label titleLabel	"MD2 Exporter v1.01"
		HyperLink addy "by Adam Barton" align:#center address:"mailto:adam.add3d.co.uk" color:(color 0 100 0) hoverColor:(color 0 0 100)
		HyperLink me "ADD3D" align:#center address:"www.add3d.co.uk" color:(color 0 100 0) hoverColor:(color 0 0 100)

	)
	
	on bexport pressed do
		( 
		md2filename=getsavefilename caption:"Save MD2" \
			filename:md2lastfile \
			types:"Blitz MD2 (*.md2)|*.md2|All Files (*.*)|*.*|"
		if md2filename!=undefined do 
			(				
			addcheck (writemd2 $ md2filename)
			md2lastfile=md2filename
			)
		)
)
--print nlist.count