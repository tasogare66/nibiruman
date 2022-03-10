-- title:  Nibiruman:2080
-- author: tasogare66
-- desc:   intense arcade-style shooter
-- script: lua

-- https://github.com/tasogare66/nibiruman

local insert=table.insert
local remove=table.remove
local pairs=pairs
local min,max,abs=math.min,math.max,math.abs
local sin,cos,atan2=math.sin,math.cos,math.atan2
local random=math.random
local yield=coroutine.yield
EPSILON=1e-05
local DEG2RAD = math.pi/180
local RAD2DEG = 180/math.pi
local FRAME2SEC=1/60
function table.clone(org)
	local copy={}
	for orig_key, orig_value in pairs(org) do
		copy[orig_key] = orig_value
	end
	return copy
end

GAME_VER="v.0.9.0"
SCR_WIDTH=240
SCR_HEIGHT=136

function clamp(value, _min, _max)
	return value < _min and _min or min(value,_max)
end

function print_hcenter(str,y,col,fixed,scl)
	col = col or 15
	fixed = fixed or false
	scl = scl or 1
	local width=print(str,0,-48,col,fixed,scl)
	local x=(SCR_WIDTH-width)//2
	print(str,x,y,col,fixed,scl)
	return x
end
function print_dbg(str,x,y)
	print(str,x,y,15,true,1,true)
end
KEY={B=2,H=8,K=11,O=15,P=16,
	W=23,A=1,S=19,D=4,
}
function dbg_key(k)
	return _DEBUG and key(k) or false
	--return key(k)
end
function btn_dec()
	return btnp(4)
end
function randomf(l,r)
	return l+random()*(r-l)
end
function psfx(id,dur,chnl)
	--0:shot,1:def,2:ene,3:force
	if _MUTE~=true then sfx(id,nil,dur,chnl) end
end

-- Vec2 is
-- Copyright (c) 2015 , 蒙占志(topameng) topameng@gmail.com
-- All rights reserved.
-- Use, modification and distribution are subject to the "New BSD License"
-- as listed at <url: http://www.opensource.org/licenses/bsd-license.php >.
-- https://github.com/topameng/CsToLua/blob/master/tolua/Assets/Lua/Vector2.lua
local sqrt = math.sqrt
local rawset = rawset
local rawget = rawget

Vec2 = {}
setmetatable(Vec2, Vec2)

local fields = {}
Vec2.__index = function(t,k)
	local var = rawget(Vec2, k)
	if var == nil then
		var = rawget(fields, k)
		if var ~= nil then
			return var(t)
		end
	end
	return var
end

function Vec2.new(x, y)
	local v = {x = x or 0, y = y or 0}
	setmetatable(v, Vec2)
	return v
end

function Vec2:Set(x,y)
	self.x = x or 0
	self.y = y or 0
end

function Vec2:Get()
	return self.x, self.y
end

function Vec2:SqrMagnitude()
	return self.x * self.x + self.y * self.y
end

function Vec2:Clone()
	return Vec2.new(self.x, self.y)
end

function Vec2:Normalize()
	local v = self:Clone()
	return v:SetNormalize()
end

function Vec2:SetNormalize()
	local num = self:Magnitude()
	if num == 1 then
		return self
		elseif num > 1e-05 then
				self:Div(num)
		else    
				self:Set(0,0)
	end 
	return self
end

function Vec2.Dot(lhs, rhs)
	return lhs.x * rhs.x + lhs.y * rhs.y
end
function Vec2.Cross(a,b)
	return a.x*b.y - a.y*b.x
end
function Vec2.Angle(a,b)
	local am = a:Magnitude()
	local bm = b:Magnitude()
	return math.acos(clamp(Vec2.Dot(a,b)/(am * bm),-1,1))
end

function Vec2.Magnitude(v2)
	return sqrt(v2.x * v2.x + v2.y * v2.y)
end

function Vec2:Div(d)
	self.x = self.x / d
	self.y = self.y / d
	return self
end

function Vec2:Mul(d)
	self.x = self.x * d
	self.y = self.y * d
	return self
end

function Vec2:Add(b)
	self.x = self.x + b.x
	self.y = self.y + b.y
	return self
end

function Vec2:Sub(b)
	self.x = self.x - b.x
	self.y = self.y - b.y
	return
end

function Vec2:SetRotate(ang)
	local c=cos(ang)
	local s=sin(ang)
	local x,y=self.x,self.y
	self.x = x*c-y*s
	self.y = y*c+x*s
	return self
end
function Vec2:Rotate(ang)
	local v = self:Clone()
	return v:SetRotate(ang)
end

Vec2.__tostring = function(self)
	return string.format("[%f,%f]", self.x, self.y)
end

Vec2.__div = function(va, d)
	return Vec2.new(va.x / d, va.y / d)
end

Vec2.__mul = function(va, d)
	return Vec2.new(va.x * d, va.y * d)
end

Vec2.__add = function(va, vb)
	return Vec2.new(va.x + vb.x, va.y + vb.y)
end

Vec2.__sub = function(va, vb)
	return Vec2.new(va.x - vb.x, va.y - vb.y)
end

Vec2.__unm = function(va)
	return Vec2.new(-va.x, -va.y)
end

Vec2.__eq = function(va,vb)
	return va.x == vb.x and va.y == vb.y
end

fields.up     = function() return Vec2.new(0,1) end
fields.right  = function() return Vec2.new(1,0) end
fields.zero   = function() return Vec2.new(0,0) end
fields.one    = function() return Vec2.new(1,1) end

fields.magnitude    = Vec2.Magnitude
fields.normalized   = Vec2.Normalize
fields.sqrMagnitude = Vec2.SqrMagnitude

--// matrix //
--[[
LuaMatrix
Licensed under the same terms as Lua itself.
Developers:
	Michael Lutz (chillcode) - original author
	David Manura http://lua-users.org/wiki/DavidManura

	https://github.com/davidm/lua-matrix/blob/master/LICENSE.txt
--]]
local matrix = {_TYPE='module', _NAME='matrix', _VERSION='0.2.11.20120416'}

local matrix_meta = {}

function matrix:new( rows, columns, value )
	if type( rows ) == "table" then
		if type(rows[1]) ~= "table" then -- expect a vector
			return setmetatable( {{rows[1]},{rows[2]},{rows[3]}},matrix_meta )
		end
		return setmetatable( rows,matrix_meta )
	end
	local mtx = {}
	local value = value or 0
	if columns == "I" then
		for i = 1,rows do
			mtx[i] = {}
			for j = 1,rows do
				if i == j then
					mtx[i][j] = 1
				else
					mtx[i][j] = 0
				end
			end
		end
	else
		for i = 1,rows do
			mtx[i] = {}
			for j = 1,columns do
				mtx[i][j] = value
			end
		end
	end
	return setmetatable( mtx,matrix_meta )
end

setmetatable( matrix, { __call = function( ... ) return matrix.new( ... ) end } )

function matrix.add( m1, m2 )
	local mtx = {}
	for i = 1,#m1 do
		local m3i = {}
		mtx[i] = m3i
		for j = 1,#m1[1] do
			m3i[j] = m1[i][j] + m2[i][j]
		end
	end
	return setmetatable( mtx, matrix_meta )
end

function matrix.sub( m1, m2 )
	local mtx = {}
	for i = 1,#m1 do
		local m3i = {}
		mtx[i] = m3i
		for j = 1,#m1[1] do
			m3i[j] = m1[i][j] - m2[i][j]
		end
	end
	return setmetatable( mtx, matrix_meta )
end

function matrix.mul( m1, m2 )
	local mtx = {}
	for i = 1,#m1 do
		mtx[i] = {}
		for j = 1,#m2[1] do
			local num = m1[i][1] * m2[1][j]
			for n = 2,#m1[1] do
				num = num + m1[i][n] * m2[n][j]
			end
			mtx[i][j] = num
		end
	end
	return setmetatable( mtx, matrix_meta )
end

function matrix.mulnum( m1, num )
	local mtx = {}
	for i = 1,#m1 do
		mtx[i] = {}
		for j = 1,#m1[1] do
			mtx[i][j] = m1[i][j] * num
		end
	end
	return setmetatable( mtx, matrix_meta )
end

function matrix.divnum( m1, num )
	local mtx = {}
	for i = 1,#m1 do
		local mtxi = {}
		mtx[i] = mtxi
		for j = 1,#m1[1] do
			mtxi[j] = m1[i][j] / num
		end
	end
	return setmetatable( mtx, matrix_meta )
end

function matrix.type( mtx )
	local e = mtx[1][1]
	if type(e) == "table" then
		if e.type then
			return e:type()
		end
		return "tensor"
	end
	return "number"
end
	
function matrix.rows( mtx )
	return #mtx
end
function matrix.columns( mtx )
	return #mtx[1]
end
function matrix.size( mtx )
	return #mtx,#mtx[1]
end

function matrix.getelement( mtx,i,j )
	if mtx[i] and mtx[i][j] then
		return mtx[i][j]
	end
end

function matrix.setelement( mtx,i,j,value )
	if matrix.getelement( mtx,i,j ) then
		-- check if value type is number
		mtx[i][j] = value
		return 1
	end
end

function matrix.replace( m1, func, ... )
	local mtx = {}
	for i = 1,#m1 do
		local m1i = m1[i]
		local mtxi = {}
		for j = 1,#m1i do
			mtxi[j] = func( m1i[j], ... )
		end
		mtx[i] = mtxi
	end
	return setmetatable( mtx, matrix_meta )
end

matrix_meta.__add = function( ... )
	return matrix.add( ... )
end

matrix_meta.__sub = function( ... )
	return matrix.sub( ... )
end

matrix_meta.__mul = function( m1,m2 )
	if getmetatable( m1 ) ~= matrix_meta then
		return matrix.mulnum( m2,m1 )
	elseif getmetatable( m2 ) ~= matrix_meta then
		return matrix.mulnum( m1,m2 )
	end
	return matrix.mul( m1,m2 )
end

matrix_meta.__unm = function( mtx )
	return matrix.mulnum( mtx,-1 )
end

matrix_meta.__eq = function( m1, m2 )
	-- check same type
	if matrix.type( m1 ) ~= matrix.type( m2 ) then
		return false
	end
	-- check same size
	if #m1 ~= #m2 or #m1[1] ~= #m2[1] then
		return false
	end
	-- check elements equal
	for i = 1,#m1 do
		for j = 1,#m1[1] do
			if m1[i][j] ~= m2[i][j] then
				return false
			end
		end
	end
	return true
end

matrix_meta.__index = {}
for k,v in pairs( matrix ) do
	matrix_meta.__index[k] = v
end

local symbol_meta = {}; symbol_meta.__index = symbol_meta
local symbol = symbol_meta

function symbol_meta.new(o)
	return setmetatable({tostring(o)}, symbol_meta)
end
symbol_meta.to = symbol_meta.new

setmetatable( symbol_meta, { __call = function( _,s ) return symbol_meta.to( s ) end } )

function symbol_meta.tostring( e,fstr )
	return string.format( fstr,e[1] )
end

function symbol_meta:type()
	if getmetatable(self) == symbol_meta then
		return "symbol"
	end
end

function symbol_meta:gsub(from, to)
	return symbol.to( string.gsub( self[1],from,to ) )
end

function symbol_meta.makereplacer( ... )
	local tosub = {}
	local args = {...}
	for i = 1,#args,2 do
		tosub[args[i]] = args[i+1]
		end
	local function func( a ) return tosub[a] or a end
	return function(sym)
		return symbol.to( string.gsub( sym[1], "%a", func ) )
	end
end

function symbol_meta.abs(a)
	return symbol.to("(" .. a[1] .. "):abs()")
end

function symbol_meta.sqrt(a)
	return symbol.to("(" .. a[1] .. "):sqrt()")
end

function symbol_meta.__add(a,b)
	return symbol.to(a .. "+" .. b)
end

function symbol_meta.__sub(a,b)
	return symbol.to(a .. "-" .. b)
end

function symbol_meta.__mul(a,b)
	return symbol.to("(" .. a .. ")*(" .. b .. ")")
end

function symbol_meta.__eq(a,b)
	return a[1] == b[1]
end

function symbol_meta.__tostring(a)
	return a[1]
end

function symbol_meta.__concat(a,b)
	return tostring(a) .. tostring(b)
end

matrix.symbol = symbol

--return matrix

-- shash.lua
--
-- Copyright (c) 2017 rxi
--
-- This library is free software; you can redistribute it and/or modify it
-- under the terms of the MIT license. See LICENSE for details.

-- https://github.com/rxi/shash/blob/master/LICENSE

local shash = { _version = "0.1.1" }
shash.__index = shash

function shash.new(cellsize)
	local self = setmetatable({}, shash)
	cellsize = cellsize or 64
	self.cellsize = cellsize
	self.tablepool = {}
	self.cells = {}
	self.entities = {}
	self.numentities=0
	return self
end

local function coord_to_key(x, y)
	return x + y * 1e7
end

local function cell_position(cellsize, x, y)
	return math.floor(x / cellsize), math.floor(y / cellsize)
end

local function each_overlapping_cell(self, e, fn, ...)
	local cellsize = self.cellsize
	local sx, sy = cell_position(cellsize, e[1], e[2])
	local ex, ey = cell_position(cellsize, e[3], e[4])
	for y = sy, ey do
		for x = sx, ex do
			local idx = coord_to_key(x, y)
			fn(self, idx, ...)
		end
	end
end

local function add_entity_to_cell(self, idx, e)
	if not self.cells[idx] then
		self.cells[idx] = { e }
	else
		insert(self.cells[idx], e)
	end
end

local function remove_entity_from_cell(self, idx, e)
	local t = self.cells[idx]
	local n = #t
	-- Only one entity? Remove entity from cell and remove cell
	if n == 1 then
		self.cells[idx] = nil
		return
	end
	-- Find and swap-remove entity
	for i, v in ipairs(t) do
		if v == e then
			t[i] = t[n]
			t[n] = nil
			return
		end
	end
end

function shash:add(obj, x, y, w, h)
	-- Create entity. The table is used as an array as this offers a noticable
	-- performance increase on LuaJIT; the indices are as follows:
	-- [1] = left, [2] = top, [3] = right, [4] = bottom, [5] = object
	local e = { x, y, x + w, y + h, obj }
	-- Add to main entities table
	self.entities[obj] = e
	self.numentities=self.numentities+1
	-- Add to cells
	each_overlapping_cell(self, e, add_entity_to_cell, e)
end

function shash:remove(obj)
	-- Get entity of obj
	local e = self.entities[obj]
	-- Remove from main entities table
	self.entities[obj] = nil
	self.numentities=self.numentities-1
	-- Remove from cells
	each_overlapping_cell(self, e, remove_entity_from_cell, e)
end

function shash:update(obj, x, y, w, h)
	-- Get entity from obj
	local e = self.entities[obj]
	-- No width/height specified? Get width/height from existing bounding box
	w = w or e[3] - e[1]
	h = h or e[4] - e[2]
	-- Check the entity has actually changed cell-position, if it hasn't we don't
	-- need to touch the cells at all
	local cellsize = self.cellsize
	local ax1, ay1 = cell_position(cellsize, e[1], e[2])
	local ax2, ay2 = cell_position(cellsize, e[3], e[4])
	local bx1, by1 = cell_position(cellsize, x, y)
	local bx2, by2 = cell_position(cellsize, x + w, y + h)
	local dirty = ax1 ~= bx1 or ay1 ~= by1 or ax2 ~= bx2 or ay2 ~= by2
	-- Remove from old cells
	if dirty then
		each_overlapping_cell(self, e, remove_entity_from_cell, e)
	end
	-- Update entity
	e[1], e[2], e[3], e[4] = x, y, x + w, y + h
	-- Add to new cells
	if dirty then
		each_overlapping_cell(self, e, add_entity_to_cell, e)
	end
end

function shash:clear()
	-- Clear all cells and entities
	for k in pairs(self.cells) do
		self.cells[k] = nil
	end
	for k in pairs(self.entities) do
		self.entities[k] = nil
	end
end

local function overlaps(e1, e2)
	return e1[3] > e2[1] and e1[1] < e2[3] and e1[4] > e2[2] and e1[2] < e2[4]
end

local function each_overlapping_in_cell(self, idx, e, set, fn, ...)
	local t = self.cells[idx]
	if not t then
		return
	end
	for i, v in ipairs(t) do
		if e ~= v and overlaps(e, v) and not set[v] then
			fn(v[5], ...)
			set[v] = true
		end
	end
end

local function each_overlapping_entity(self, e, fn, ...)
	-- Init set for keeping track of which entities have already been handled
	local set = remove(self.tablepool) or {}
	-- Do overlap checks
	each_overlapping_cell(self, e, each_overlapping_in_cell, e, set, fn, ...)
	-- Clear set and return to pool
	for v in pairs(set) do
		set[v] = nil
	end
	insert(self.tablepool, set)
end

function shash:each(x, y, w, h, fn, ...)
	local e = self.entities[x]
	if e then
		-- Got object, use its entity
		each_overlapping_entity(self, e, y, w, h, fn, ...)
	else
		-- Got bounding box, make temporary entity
		each_overlapping_entity(self, { x, y, x + w, y + h }, fn, ...)
	end
end

function shash:info(opt, ...)
	if opt == "cells" or opt == "entities" then
		local n = 0
		for k in pairs(self[opt]) do
			n = n + 1
		end
		return n
	end
	if opt == "cell" then
		local t = self.cells[ coord_to_key(...) ]
		return t and #t or 0
	end
	error( string.format("invalid opt '%s'", opt) )
end


Button_Up=1<<0
Button_Down=1<<1
Button_Left=1<<2
Button_Right=1<<3
Button_Shot=1<<4
Button_Dash=1<<5
IData={}
function IData.new()
	local d={mask=0,mpos=Vec2.new(),dt=0,num=0}
	setmetatable(d,{__index=IData})
	return d
end
function IData:upd(dt)
	local m=0
	if btn(0) or key(KEY.W) then m=m|Button_Up end
	if btn(1) or key(KEY.S) then m=m|Button_Down end
	if btn(2) or key(KEY.A) then m=m|Button_Left end
	if btn(3) or key(KEY.D) then m=m|Button_Right end
	local mx,my,ml,_,mr=mouse()
	if btn(4) or ml then m=m|Button_Shot end
	if btn(5) or mr then m=m|Button_Dash end
	local mpos=Vec2.new(mx,my)
	self.mask=m
	self.mpos=mpos
	self.dt=dt
	self.num=self.num+1
end
function IData.eq(a,b)
	return (a.dt==b.dt and a.mask==b.mask and a.mpos==b.mpos)
end

Input={
	StateLog=1,StateTrace=2,
	cur=IData.new(),
	logs={},
	state=0,--1:log,2:trace
	init_flag=false,
	seed=0,
	seed_bk=0,
	pos=1,num=0,
}
function getseed()
	return random(-2147483648,2147483647)
end
function Input:start(s)
	if s==Input.StateTrace and #self.logs<=0 then s=Input.StateTrace end
	self.state=s
	self.init_flag=true
end
function Input:term()
	if self.state==self.StateLog then
		insert(self.logs,self.cur)
	elseif self.state==self.StateTrace then
		math.randomseed(self.seed_bk) --seed戻す
	end
	self.cur=IData.new()
	self.state=0
end
function Input:upd_start()
	if self.state==self.StateLog then
		self.logs={}
		self.cur=IData.new()
		self.seed=getseed()
	elseif self.state==self.StateTrace then
		self.seed_bk=getseed()
		self.pos=1
		self.num=0
	end
	--trace("seed:"..self.seed)
	math.randomseed(self.seed)
end
function Input:upd(dt)
	if self.init_flag then
		self:upd_start()
		self.init_flag=false
	end

	if self.state==self.StateLog then
		local d=IData.new()
		d:upd(dt)
		if self.cur.num==0 then
			self.cur=d
		elseif IData.eq(self.cur,d) then
			self.cur.num=self.cur.num+1
		else
			insert(self.logs,self.cur)
			self.cur=d
		end
	elseif self.state==self.StateTrace then
		if self.cur.num <= self.num then
			self.pos=self.pos+1
			self.num=0
		end
		if self.pos<=#self.logs then
			self.cur=self.logs[self.pos]
			dt=self.cur.dt
			self.num=self.num+1
		else
			self.cur=IData.new() --error
		end
	else
		self.cur:upd(dt)
	end
	return dt
end
function Input:on(b)
	return self.cur.mask&b~=0
end
function Input:mouse()
	return self.cur.mpos
end
function Input:log_num()
	return #self.logs
end
function Input:exists_log()
	return #self.logs>0
end

function lerp(a,b,t) return a*(1-t) + b*t end
Camera={
	trs=Vec2.new(),
	inr_center=Vec2.new(0,0), --inner center
	center=Vec2.new(0,0),
	top_left=Vec2.new(),
	shake=0,
}
function Camera:upd(t)
	local p=t.pos
	local cam=self
	cam.inr_center.x=clamp(lerp(cam.inr_center.x,p.x,0.2),-80,80)
	cam.inr_center.y=clamp(lerp(cam.inr_center.y,p.y,0.2),-140,130)
	cam.center = cam.inr_center + self:upd_shake()
	cam.top_left=cam.center-Vec2.new(120,64)
	cam.trs=-cam.top_left
end
function Camera:upd_shake()
	local ofs=Vec2.new(0,0)
	if self.shake<=0 then return ofs end
	ofs.x=16-random(32)
	ofs.y=16-random(32)
	ofs:Mul(self.shake)
	local fade = 0.95
	self.shake = self.shake*fade
  if self.shake<0.05 then self.shake=0 end
	return ofs
end
function Camera:req_shake(v)
	self.shake=min(self.shake+v,4)
end
function Camera:calc_cc(r)
	local p=self.center*r-Vec2.new(120,64)
	local ccx=math.floor(p.x/8)
	local ccy=math.floor(p.y/8)
	local ofs=Vec2.new(ccx*8,ccy*8)-p
	return ccx,ccy,ofs.x,ofs.y
end
function Camera:reset()
	self.inr_center=Vec2.new(0,0)
	self.center=Vec2.new(0,0)
	self.shake=0
end

-- particle2
SqpLst={
	lst={},
}
function SqpLst:clear()
	self.lst={}
end
function SqpLst:add(p,num,radius)
	self:add_base(p,num,radius,0.7, {
		cols={3,7},
		sidea=3,sideb=10,
	})
end
function SqpLst:add_bossap(p,num,radius)
	self:add_base(p,num,radius,0.7, {
		cols={6,9,15},
		sidea=3,sideb=12,
		side_ed_a=1,side_ed_b=5
	})
end
function SqpLst:add_base(p,num,radius,lifetm,args)
	local cols=args.cols
	for i=1,num do
		local r=sqrt(random())*radius
		local theta=randomf(-math.pi,math.pi)
		local ofs=Vec2.new(r*cos(theta),r*sin(theta))
		local side=random(args.sidea,args.sideb) --size
		local side_ed=args.side_ed_a and min(random(args.side_ed_a,args.side_ed_b),side) or side
		local pt={
			pos=p+ofs,
			col=cols[random(1,#cols)],
			wh=side,
			whhalf=side/2,
			whst=side,
			whed=side_ed,
			dir=Vec2.new(cos(theta),sin(theta))*random(4,12),
			lifetm=lifetm,
			lifetm_base=lifetm,
		}
		insert(self.lst,#self.lst+1,pt)
	end
end
function SqpLst:update(dt)
	local lst=self.lst
	local ptcl 
	local p,cnt=1,0 --削除数
	for i=1,#lst do
		ptcl=lst[i]
		ptcl.lifetm=ptcl.lifetm-dt
		if(ptcl.lifetm>0)then
			ptcl.pos=ptcl.pos+ptcl.dir*dt
			ptcl.wh=lerp(ptcl.whed,ptcl.whst,ptcl.lifetm/ptcl.lifetm_base)
			ptcl.whhalf=ptcl.wh/2
			lst[p]=ptcl
			p=p+1
		else
			cnt=cnt+1
		end
	end
	for i=1,cnt do
		lst[#lst]=nil
	end
end
function SqpLst:draw(cam)
	for _,p in pairs(self.lst) do
		rect(p.pos.x-p.whhalf+cam.x,p.pos.y-p.whhalf+cam.y,p.wh,p.wh,p.col)
	end
end

-- particle
PtclLst={
	lst={},
}
function PtclLst:clear()
	self.lst={}
	SqpLst:clear()
end
function PtclLst:add(p,col)
	for i=1,6 do
		local theta = randomf(-math.pi, math.pi)
		local p={
			pos=p,
			col=col,
			dir=Vec2.new(cos(theta),sin(theta))*random(15,24),
			lifetm=1,
		}
		insert(self.lst,#self.lst+1,p)
	end
end
function PtclLst:update(dt)
	local lst=self.lst
	local ptcl 
	local p,cnt=1,0 --削除数
	for i=1,#lst do
		ptcl=lst[i]
		ptcl.lifetm=ptcl.lifetm-dt
		if(ptcl.lifetm>0)then
			ptcl.pos=ptcl.pos+ptcl.dir*dt
			lst[p]=ptcl
			p=p+1
		else
			cnt=cnt+1
		end
	end
	for i=1,cnt do
		lst[#lst]=nil
	end
	SqpLst:update(dt)
end
function PtclLst:draw(cam)
	SqpLst:draw(cam)
	for _,p in pairs(self.lst) do
		pix(p.pos.x+cam.x,p.pos.y+cam.y,p.col)
	end
end
function PtclLst:count()
	return #self.lst+#SqpLst.lst
end

function rspr(sx,sy,scale,angle,mx,my,mw,mh)
	--this is fixed , to make a textured quad
	--X , Y , U , V
	local sv ={{-1,-1, 0,0},
						{ 1,-1, 0.999,0},
						{-1,1,  0,0.999},
						{ 1,1,  0.999,0.999}}
	--  the scale is mw ( map width ) * 4 * scale 
	--  mapwidth is * 4 because we actually want HALF width to center the image
	local scalex = (mw<<2)*scale
	local scaley = (mh<<2)*scale
	local ca,sa=cos(angle),sin(angle)
	--  rotate the quad points
	for p=1,#sv do 
		-- apply scale
		local _sx = sv[p][1] * scalex 
		local _sy = sv[p][2] * scaley
		-- apply rotation
		local rx = _sx*ca -_sy*sa
		local ry = _sx*sa +_sy*ca
		-- apply transform
		sv[p][1] = rx+sx
		sv[p][2] = ry+sy
		-- scale UV's 
		sv[p][3] = (mx<<3) + (sv[p][3] * (mw<<3))
		sv[p][4] = (my<<3) + (sv[p][4] * (mh<<3))
	end
	textri( sv[1][1],sv[1][2], sv[2][1],sv[2][2], sv[3][1],sv[3][2],
					sv[1][3],sv[1][4], sv[2][3],sv[2][4], sv[3][3],sv[3][4], false,0)--use_map,colorkey
	textri( sv[2][1],sv[2][2], sv[3][1],sv[3][2], sv[4][1],sv[4][2],
					sv[2][3],sv[2][4], sv[3][3],sv[3][4], sv[4][3],sv[4][4], false,0)
end

-- eintity
Flag_del=1<<0
Flag_Verlet=1<<1
Flag_px=1<<2
Flag_bullet=1<<3
Flag_EneBullet=1<<4
Flag_Force=1<<5
Flag_player=1<<6
Flag_Ally=1<<7
Flag_Invincible=1<<8
Flag_HaveDot=1<<9
Flag_Spawned=1<<10
BOUNCE=0.5
HitMask_Enemy=1<<0
HitMask_Player=1<<1
Entity={}
Entity.new=function(ino,ix,iy,iradius,imass)
	iradius = iradius or 3
	imass = imass or 1 -- default:1
	local base = {
		pos=Vec2.new(ix,iy),
		old_pos=Vec2.new(ix,iy),
		vel=Vec2.new(),
		radius=iradius,
		force=Vec2.new(),
		mov=Vec2.new(),
		mov_old=Vec2.new(),
		mass=imass, inv_mass=1/imass,
		aabb0=Vec2.new(ix-iradius, iy-iradius),
		aabb_size=Vec2.new(iradius*2,iradius*2),
		half_extents=Vec2.new(iradius,iradius),
		sha=nil,
		flag=0,
		no=ino,
		health=1,
		exp_resi=1,
		hit_mask=0, --HitMask
		colli_attr=0, --hit時通知するattribute
	}
	return setmetatable(base,{__index=Entity})
end
function Entity:set_radius(ir)
	self.radius=ir
	self.aabb_size:Set(ir*2,ir*2)
	self.half_extents:Set(ir,ir)
	self.aabb0=self.pos-self.half_extents
end
function Entity:set_mass(imass)
	self.mass=imass
	self.inv_mass=1/imass
end
Entity.set_position=function(self,ipos)
	self.pos=ipos
	--updateAABB
	self.aabb0=self.pos-self.half_extents
	if self.sha then
		self.sha:update(self, self.aabb0.x, self.aabb0.y, self.aabb_size.x, self.aabb_size.y)
	end
end
Entity.get_estimate_pos=function(self)
	return self.pos+self.mov
end
Entity.updateEstimateAABB=function(self)
	--movも含めて更新
	self.aabb0=self.pos+self.mov-self.half_extents
end
Entity.calc_velocity=function(self)
	return self.pos-self.old_pos
end
Entity.set_velocity=function(self,ivel)
	self.vel=ivel
end
function Entity:add_vel_force(v)
	self.old_pos:Sub(v)
end
function Entity:set_vel_force(v)
	self.old_pos=self.pos-v
end
function Entity:lim_vel_force(l)
	local v=self:calc_velocity()
	local len=v:Magnitude()
	if len>l then
		self.old_pos=self.pos-v*l
	end
end
Entity.repulse=function(self,inml,dist)
	local bounce=1.0+BOUNCE
	self.mov=self.mov + (inml * (-dist) * bounce) -- * (1.0 + bounce[i])); --bounce:1
	local dot_vel_n=Vec2.Dot(self.vel,inml)
	self.mov_old=self.mov_old + inml * dot_vel_n * bounce -- + dot_vel_n * inml-- * bounce
end
Entity.pre_pro=function(self)
	self.force:Set(0,0) --9*self.mass
	self.mov:Set(0,0)
	self.mov_old:Set(0,0)
	self:set_velocity(self:calc_velocity())
end
Entity.do_verlet=function(self,dt,inv_prev_dt,decel)
	local pos=self.pos
	local tmp=self.old_pos
	pos=pos+self.mov
	tmp=tmp+self.mov
	tmp=tmp+self.mov_old --change velocity
	-- do verlet
	local vel=(pos-tmp)*inv_prev_dt
	local inv_mass_dt=self.inv_mass*dt
	vel=vel+self.force*inv_mass_dt
	vel=vel*decel

	tmp=pos+vel*dt

	self:set_position(tmp)
	self.old_pos = pos
	self:set_velocity(vel)
end
function Entity:topleft(cam)
	local p=self.pos+cam
	p.x=p.x+0.5-4
	p.y=p.y+0.5-4
	return p
end
Entity.drw=function(self,cam)
	local c=self:topleft(cam)
	spr(self.spr,c.x,c.y,0)
	--circ(p.x+0.5,p.y+0.5,self.radius,6)
	--print(string.format("%5.2f,%5.2f",self.pos.x,self.pos.y), p.x, p.y)
end
function Entity:drw_blink(cam)
	local c=self:topleft(cam)
	local s=self:is_blink() and 267 or self.spr
	spr(s,c.x,c.y,0)
end
Entity.del=function(self)
	self.flag = self.flag | Flag_del
end
Entity.check_flag=function(self,f)
	return (self.flag & f ~= 0)
end
Entity.on_flag=function(self,f)
	self.flag = self.flag | f
end
Entity.off_flag=function(self,f)
	self.flag = self.flag & (~f)
end
Entity.chk_hitmask=function(self,m)
	return (self.hit_mask & m ~= 0)
end
Entity.off_hitmask=function(self,m)
	self.hit_mask = self.hit_mask & (~m)
end
Entity.sub_health_dmg=function(self,dmg)
	if self:check_flag(Flag_del) then return end
	self.health = self.health - dmg
	if self.health <= 0 then
	 	self:del()
	 	if self.dead then self:dead() end
	else
		self:set_blink()
	end
end
Entity.sub_health=function(self,t)
	self:sub_health_dmg(t.health)
end

-- player
Player={ 
	reticle=nil,
	base_flag=Flag_px|Flag_player|Flag_Invincible,
	shot_repeat=0,
	invincible_time=0,
	dash_limit=2.2,
	dash_coolt=4.2,
	dash_followt=0.9,
}
setmetatable(Player,{__index=Entity})
Player.init=function(self)
	self.colli_attr=HitMask_Player
	self.dashst=0
	self.dasht=0
	self.flwt=0
	self.coolt=0
	self.dashvec=Vec2.new(0,1)
	self.dashvec_old=Vec2.new(0,0)
	self.capradius=20
	self.armslv=1
	self.armstime=0
	self.elp=0
	self.animcnt=0
	self.animdir=0
	self.active=true
end
function Player:set_active(flg)
	self.active=flg
end
function Player:is_dash()
	return self.dashst~=0
end
function Player:is_dashing()
	return self.dashst==1
end
function Player:dash_pow()
	return 1-max(self.dasht/self.dash_limit,0)
end
function Player:reset_dash()
	self.flwt=0
	self.coolt=0
end
Player.check_dead=function(self)
	if self:chk_hitmask(HitMask_Enemy) then
		self:off_hitmask(HitMask_Enemy)
		if not self:is_dash() then
			Camera:req_shake(1.4)
			if GAME:decriment_life()>0 then
				GAME:reset_multiplier()
				self:reset_dash()
				ObjLstA:add(self.pos.x,self.pos.y,ForceF)
			end
			self:set_invincible()
			GAME:reduceDiff(300)
			return true
		end
	end
	return false
end
Player.set_invincible=function(self)
	self:on_flag(Flag_Invincible)
	self.invincible_time=0
end
Player.check_invincible=function(self,dt)
	if self:check_flag(Flag_Invincible) then
		if self.invincible_time > 3 then --3秒
			self:off_flag(Flag_Invincible)
		end
		self.invincible_time = self.invincible_time + dt
	end
end
function Player:add_armslv()
	self.armslv=min(self.armslv+1,3)
	self.armstime=18
end
function Player:upd_armslv(dt)
	if self.armstime>0 then
		self.armstime=self.armstime-dt
	else
		self.armslv=1
	end
end
Player.upd=function(self,dt)
	--upd anim
	self.animcnt=(self.elp//(FRAME2SEC*10)%4)
	self.elp=self.elp+dt
	if not self.active then return end
	local chara_dir=self.reticle.pos-self.pos
	self.animdir=(chara_dir.x>0) and 0 or 1
	self.animdir=self.animdir|((chara_dir.y<0) and 2 or 0)
	if self:check_dead() then return end
	self:check_invincible(dt)
	self:upd_armslv(dt)

	local v=Vec2.new(0,0)
	if Input:on(Button_Up) then v.y=v.y-1 end
	if Input:on(Button_Down) then v.y=v.y+1 end
	if Input:on(Button_Left) then v.x=v.x-1 end
	if Input:on(Button_Right) then v.x=v.x+1 end
	v:SetNormalize()
	self.mov = v*(60*dt)
	--dash
	local dashon=Input:on(Button_Dash)
	if self:check_flag(Flag_Invincible) and self.invincible_time<1 then dashon=false end --ignore start invincible
	if self.dashst==0 then
		if self.coolt > 0 then
			self.coolt=self.coolt-dt
			if self.coolt<=0 then psfx(9,40,3) end 
		else
			if dashon then
				local v=self.reticle.pos-self.pos
				local len=v:Magnitude()
				if(len>EPSILON)then
					v:Div(len)
					Camera:req_shake(0.2)
					self:set_vel_force(v*6.8)
					self.dashvec=v
					self.dashvec_old=v
					self.dasht=self.dash_limit
					self.dashst=1
				end
			end
		end
	elseif self.dashst==1 then
		self.dasht=self.dasht-dt
		self.dashvec=self.pos-self.old_pos
		self.dashvec:SetNormalize()
		if (self.elp//FRAME2SEC)%10==0 then
			psfx(8,10,0)
		end

		if self.dash_limit-self.dasht<0.1 or (dashon and self.dasht>=0) then
			self:set_vel_force(self.dashvec*6.8)
			if v.x~=0 or v.y~=0 then
				local ang=Vec2.Angle(self.dashvec,v)
				local dif=min(ang,math.rad(1))
				if(Vec2.Cross(self.dashvec,v)<0)then
					dif=-dif
				end
				self.dashvec:SetRotate(dif)
			end
			local th=Vec2.Dot(self.dashvec,self.dashvec_old)
			if th<=0.5 then --cos60°
				ObjLstA:add(self.pos.x,self.pos.y,ForceF,{pow=self:dash_pow()})
				Camera:req_shake(0.3)
			else
				ObjLstA:add(self.pos.x,self.pos.y,ForceD)
			end
		else
			ObjLstA:add(self.pos.x,self.pos.y,ForceF,{pow=self:dash_pow()})
			Camera:req_shake(0.3)
			self:set_vel_force(self.dashvec*0.2)
			self.flwt=self.dash_followt
			self.coolt=self.dash_coolt
			self.dashst=2
		end
		self.dashvec_old=self.dashvec
	elseif self.dashst>=2 then --invincible
		self.flwt=self.flwt-dt
		self:lim_vel_force(0.2)
		if self.flwt<0 then
			self.dashst=0
		end
	end
	--shot
	if Input:on(Button_Shot) and not self:is_dashing() then
		if self.shot_repeat == 0 then
			local v=self.reticle.pos-self.pos
			local d=v:Magnitude()
			if d > EPSILON then
				ObjLstA:add(self.pos.x,self.pos.y,PlBullet,{dir=v:Div(d)})
				if self.armslv>1 then
					local ang=math.rad(20)
					local c,s=cos(ang),sin(ang)
					ObjLstA:add(self.pos.x,self.pos.y,PlBullet,{dir=Vec2.new(v.x*c-v.y*s,v.y*c+v.x*s)})
					ObjLstA:add(self.pos.x,self.pos.y,PlBullet,{dir=Vec2.new(v.x*c+v.y*s,v.y*c-v.x*s)})
				end
				psfx(3,9,0)
			end
		end
		self.shot_repeat=self.shot_repeat+1
		local shot_repeat_cnt={4,6,5}
		if self.shot_repeat > shot_repeat_cnt[self.armslv] then self.shot_repeat=0 end
	else
		self.shot_repeat=0
	end
	-- if btnp(6) then
	-- 	ObjLstA:add(self.pos.x+1,self.pos.y+1,ForceF)
	-- end
end
Player.drw=function(self,cam)
	self.spr = 400+self.animdir*16+self.animcnt
	if self:check_flag(Flag_Invincible) and self.active then
		local r=self.invincible_time // (FRAME2SEC*6)
		if r%2==0 then self.spr=267 end
	end
	Entity.drw(self,cam)
	-- local p=self.pos+cam
	-- circb(p.x,p.y,self.capradius,6)
	--gauge
	if self:is_dash() or self.coolt>0 then
		local c=self:topleft(cam)
		local len=10
		c.y=c.y-3
		c.x=c.x-(len-8)/2
		local r,col=0,6
		if self:is_dash() then
			r=self.dasht/self.dash_limit
			col=11
		else r=1-self.coolt/self.dash_coolt end
		line(c.x,c.y,c.x+len,c.y,15)
		line(c.x,c.y,c.x+len*r,c.y,col)
	end
end

-- playr bullet
PlBullet={ elp=0, dir=Vec2.new(), base_flag=Flag_bullet|Flag_Verlet }
setmetatable(PlBullet,{__index=Entity})
PlBullet.spr=496
function PlBullet:init(args)
	self.dir=args.dir*150
	self.ang=atan2(self.dir.y,self.dir.x)
end
PlBullet.upd=function(self,dt)
	self.elp=self.elp+dt
	if self.elp >= 1 then
		self:del()
		return
	end
	self.mov = self.dir*dt
end
function PlBullet:drw(cam)
	local p=self.pos+cam
	rspr(p.x,p.y,1,self.ang,0,31,1,1)
end
-- reticle
Reticle={}
setmetatable(Reticle,{__index=Entity})
Reticle.spr=480
Reticle.upd=function(self,dt)
	self.pos = Input:mouse()+Camera.top_left
end
-- force
ForceF={elp=0,base_flag=Flag_Force}
setmetatable(ForceF,{__index=Entity})
function ForceF:init(args)
	local t=args and args.pow or 0
	self:set_radius(29)
	self.health=max(lerp(1,50,t),10)
	self.inner_r=23
	SqpLst:add(self.pos,20,self.inner_r-4)
	psfx(5,30,3)
end
function ForceF:upd(dt)
	self.elp=self.elp+dt
	if self.elp >=0.4 then
		self:del()
	end
end
function ForceF:drw(cam)
	local p=self.pos+cam
	circb(p.x,p.y,self.inner_r,6)
end
function ForceF:hitcb(o,dir,d)
	if d < o.radius+self.inner_r then
		o:sub_health_dmg(self.health*o.exp_resi)
	else
		o:add_vel_force(dir:Div(d)*2)
	end
end
-- dash force
ForceD={elp=0,base_flag=Flag_Force}
setmetatable(ForceD,{__index=Entity})
function ForceD:init()
	self:set_radius(12)
	self.health=2
	self.base_spr=268
	self.spr=self.base_spr
end
function ForceD:upd(dt)
	local lt=0.3
	self.spr=self.base_spr+min((self.elp/0.3)*3,2)
	self.elp=self.elp+dt
	if self.elp>=lt then
		self:del()
	end
end
-- function ForceD:drw(cam)
-- 	local p=self.pos+cam
-- 	circb(p.x,p.y,self.radius,6)
-- end
function ForceD:hitcb(o,dir,d)
	o:sub_health_dmg(self.health*o.exp_resi)
end
-- enemy
Enemy={ appear_flag=true, elapsed=0, blink=0 }
Enemy.blinktm=0.08
setmetatable(Enemy,{__index=Entity})
function Enemy:init()
	self.colli_attr=HitMask_Enemy
end
function Enemy:upd(dt)
	if self.appear_flag then
		self.spr=464 + (self.elapsed/0.166)%4
		if self.elapsed >= 2 then
			self:appear()
			self.appear_flag=false
		end
	else
		self:upd_ene(dt)
	end
	self.elapsed = self.elapsed + dt
end
function Enemy:appear()
	self.spr=self.spr_ene
	if self:check_flag(Flag_EneBullet) then
		ObjLstA:insert_ene_bullet(self)
	elseif not self:check_flag(Flag_px) then
		ObjLstA:insert_pxs(self)
	end
end
function Enemy:dead()
	GAME:add_score(self.score)
	if self:check_flag(Flag_HaveDot) then
		ObjLstA:add(self.pos.x,self.pos.y,EneDot)
		psfx(2,20,1)
	end
	PtclLst:add(self.pos,15)
end
function Enemy:set_blink()
	if self.blink <= 0 then
		self.blink=self.blinktm
		psfx(10,30,2)
	end
end
function Enemy:upd_blink(dt)
	if self.blink > 0 then self.blink=self.blink-dt end
end
function Enemy:is_blink()
	return self.blink>=self.blinktm/2
end

EneSnake={base_flag=Flag_HaveDot}
setmetatable(EneSnake,{__index=Enemy})
EneSnake.spr_ene=274
EneSnake.score=20
EneSnake.upd_ene=function(self,dt)
	local tgt = GAME.pl.pos
	local dir = tgt - self.pos
	dir:SetNormalize()
	local len=9*dt*GAME:getDifV(1,3)
	self.mov = self.mov + dir*len
	local s=(self.elapsed//(FRAME2SEC*4))%4
	local animdir=dir.x>0 and 0 or 1
	self.spr=self.spr_ene+s+animdir*4
end

EneGrunt={base_flag=Flag_HaveDot}
setmetatable(EneGrunt,{__index=Enemy})
EneGrunt.spr_ene=288
EneGrunt.score=10
function EneGrunt:init()
	Enemy.init(self)
	self.rad=0
end
EneGrunt.upd_ene=function(self,dt)
	self.rad = self.rad + DEG2RAD*120*dt
	local len=0.24*GAME:getDifV(1,2.5)
	self.mov.x = self.mov.x + cos(self.rad) * len
	self.mov.y = self.mov.y + sin(self.rad) * len
	local s=(self.elapsed//(FRAME2SEC*12))%4
	local animdir=self.mov.x>0 and 0 or 1
	self.spr=self.spr_ene+s+animdir*4
end

EneHulk={base_flag=Flag_HaveDot}
setmetatable(EneHulk,{__index=Enemy})
EneHulk.spr_ene=304
EneHulk.score=300
function EneHulk:init()
	Enemy.init(self)
	self.health=30
	self.exp_resi=15
	self.drw=self.drw_blink
	self:setmvtm()
	self.animdir=random(0,1)
end
function EneHulk:setmvtm()
	self.mvtm=randomf(2.5,GAME:getDifV(6,2.7))
end
function EneHulk:upd_ene(dt)
	self:upd_blink(dt)
	local s=(self.elapsed//(FRAME2SEC*8))%2
	self.spr=self.spr_ene+self.animdir*2+s
	if self.mvtm<0 then
		local dir=GAME.pl.pos-self.pos
		self.animdir=dir.x>0 and 0 or 1
		dir:SetNormalize()
		self:add_vel_force(dir*0.8)
		self:setmvtm()
	else
		self.mvtm=self.mvtm-dt
	end
end

EneArrow={ base_flag=Flag_EneBullet|Flag_HaveDot, dir=Vec2.new() }
setmetatable(EneArrow,{__index=Enemy})
EneArrow.spr_ene=23
EneArrow.score=30
EneArrow.init=function(self,args)
	Enemy.init(self)
	self.dir=args.dir:Clone()
	local l=self.dir:Magnitude()
	if l<EPSILON then
		self.dir:Set(0,-1)
	else
		self.dir:Div(l)
	end 
	self.ang=atan2(self.dir.y,self.dir.x)
	self.speed=GAME:getDifV(45,60)
end
EneArrow.upd_ene=function(self,dt)
	self.mov = self.dir*self.speed*dt
end
function EneArrow:drw(cam)
	if self.appear_flag then
		Entity.drw(self,cam)
	else
		local p=self.pos+cam
		local s=(self.elapsed//(FRAME2SEC*8))%2
		rspr(p.x,p.y,1,self.ang,s*2,self.spr_ene,1,1)
	end
end
EneArrow.hit_wall=function(self,dir)
	self.dir=dir
	self.ang=atan2(dir.y,dir.x)
end
--arrow2,反射が違う
EneArrow2={ base_flag=Flag_EneBullet|Flag_HaveDot, dir=Vec2.new(), speed=45 }
EneArrow2.spr_ene=24
setmetatable(EneArrow2,{__index=EneArrow})
function EneArrow2:hit_wall()
	self.dir:Mul(-1)
	self.ang=atan2(self.dir.y,self.dir.x)
end

EneSphe={base_flag=Flag_EneBullet|Flag_HaveDot}
setmetatable(EneSphe,{__index=Enemy})
EneSphe.spr_ene=308
EneSphe.score=40
function EneSphe:init(args)
	Enemy.init(self)
	self.health=2
	self.exp_resi=2
	self.drw=self.drw_blink
	self.rotr=self.pos:Magnitude()
	self.rdir=args.rdir or 1
	self.speed=GAME:getDifV(50,60)
end
function EneSphe:upd_ene(dt)
	self:upd_blink(dt)
	self.spr=self.spr_ene+(self.elapsed//(FRAME2SEC*4))%2
	if self.rotr>EPSILON then
		local rad=self.speed*dt/self.rotr*self.rdir
		self.mov=self.pos:Rotate(rad)-self.pos
	end
end
function EneSphe:hit_wall(dir)
end

BossBullet={ base_flag=Flag_EneBullet, dir=Vec2.new(), speed=50 }
setmetatable(BossBullet,{__index=Enemy})
BossBullet.spr_ene=336
BossBullet.score=10
function BossBullet:init(args)
	Enemy.init(self)
	self.dir=args.dir
	self.dir:Normalize()
	Enemy.appear(self)
end
function BossBullet:upd(dt)
	self.mov = self.dir*self.speed*dt
	local s=(self.elapsed//(FRAME2SEC*8))%3
	self.spr=self.spr_ene+s
	self.elapsed=self.elapsed+dt
end
function BossBullet:hit_wall(dir)
	self:del()
end

Boss={base_flag=Flag_HaveDot}
setmetatable(Boss,{__index=Enemy})
Boss.spr_ene=328
Boss.score=5000
function Boss:init()
	Enemy.init(self)
	self:set_radius(16)
	self:set_mass(5)
	self.health_max=500
	self.health=self.health_max
	self.co=nil
	self.arms_timer=0
	self.dspofs=Vec2.new()
	self.animcnt=0
	GAME.boss=self --for info disp
end
function Boss:dead()
	local num=300
	random_circle(num,0,70,function(x,y) ObjLstA:add(self.pos.x+x,self.pos.y+y,EneDot) end)
	random_circle(4,0,self.radius,function(x,y) PtclLst:add(Vec2.new(self.pos.x+x,self.pos.y+y),6) end)
	random_circle(6,3,self.radius*2,function(x,y) PtclLst:add(Vec2.new(self.pos.x+x,self.pos.y+y),15) end)
	Camera:req_shake(2)
	Enemy.dead(self)
	psfx(5,30,3)
	GAME.boss=nil
end
function Boss:upd_for_second(sec,func)
	while sec > 0 do
		yield(1)
		if(func)then func() end
		sec = sec - self.dt
	end
end
function Boss:get_dir(tgt)
	local dir = tgt-self.pos
	return dir:SetNormalize()
end
function Boss:mv_to(pos,spd)
	self.mov=self.mov+self:get_dir(pos)*spd*self.dt
end
function Boss:arms0(t,num,ofs)
	num=num or 10
	ofs=ofs or 0
	ofs=math.rad(ofs)
	if self.arms_timer > t then
		local radius=self.radius+3
		local rad_step=2*math.pi/num
		for i=0,num-1 do
			local rad=rad_step*i+ofs
			local c=math.cos(rad)
			local s=math.sin(rad)
			ObjLstA:add(self.pos.x+c*radius,self.pos.y+s*radius,BossBullet,{dir=Vec2.new(c,s)})
		end	
		self.arms_timer = self.arms_timer % t
	end
end
function Boss:upd(dt)
	Enemy.upd(self,dt)
	self:upd_blink(dt)
	self.animcnt=(self.elapsed//(FRAME2SEC*20)%2)
	if (self.elapsed//(FRAME2SEC*14)%2==0) then --random offset
		self.dspofs:Set(2-random(4),2-random(4))
	end
	if self.appear_flag then
		SqpLst:add_bossap(self.pos,2,self.radius+6)
		if self.elapsed//FRAME2SEC%20==0 then psfx(11,20,3) end
	end
end
function Boss:upd_ene(dt)
	self.dt=dt
	self.arms_timer=self.arms_timer+dt
	if self.co then
		local _,r=coroutine.resume(self.co)
		if r~=1 and r~= nil then
			assert(false,r)
		end
	else
		self.co=coroutine.create(function()
			repeat
				self.arms_timer=0
				self:upd_for_second(5,function() self:mv_to(GAME.pl.pos,15) self:arms0(0.2) end)
				local dir=self:get_dir(GAME.pl.pos)
				self:upd_for_second(2)
				self:add_vel_force(dir*3)
				self:upd_for_second(0.3) -- wait
				self.arms_timer=0
				self:upd_for_second(3,function() self:mv_to(Vec2.new(0,0),50) self:arms0(0.2) end)
				self.arms_timer=0
				self:upd_for_second(5,function() local ofs=self.elapsed//FRAME2SEC*2 self:arms0(0.16,35,ofs) end)
			until false
		end)
	end
	self.elapsed=self.elapsed+dt
end
function Boss:drw(cam)
	if self.appear_flag then return end
	local p=self.pos+cam
	local p2=p-Vec2.new(self.radius,self.radius)
	if self:is_blink() then
		spr(332,p2.x,p2.y,0,1,0,0,4,4)
	else
		spr(self.spr_ene,p2.x,p2.y,0,1,0,0,4,4)
		p2=p2+self.dspofs
		spr(392+self.animcnt*4,p2.x,p2.y,0,1,0,0,4,4)
	end
	circb(p.x,p.y,self.radius,9)
end
function Boss:drw_info()
	if self.appear_flag then
		local wstr="WARNING"
		local y=50
		local x=print_hcenter(wstr,-100)
		local ofsx=6-random(12)
		local ofsy=6-random(12)
		print(wstr,x+ofsx,y+ofsy,random(1,15))
		print(wstr,x,y,6)
	end
	local hr=self.health/self.health_max
	local len,len2=150,150*hr
	local sx=(240-len)/2
	for i=1,3 do
		line(sx,120+i,sx+len,120+i,3)
		line(sx,120+i,sx+len2,120+i,11)
		sx=sx+1
	end
end

-- human
Human={ base_flag=Flag_Ally }
Human.spr_ene=404
setmetatable(Human,{__index=Enemy})
Human.score=1000
Human.init=function(self)
	self.colli_attr=0
	self.dir=Vec2.new()
	self.animdir=2
	self.timer=-1
end
Human.upd_ene=function(self,dt)
	if self:chk_hitmask(HitMask_Player) then
		self:del()
		self:dead()
		return
	end
	if self.timer<0 then
		local s=random(1,4)
		self.animdir=s-1
		if s==1 then self.dir:Set(20,0)
		elseif s==2 then self.dir:Set(-20,0)
		elseif s==3 then self .dir:Set(0,20)
		else self.dir:Set(0,-20) end
		self.timer=random(1,3)
	else
		self.timer = self.timer-dt
	end
	self.mov = self.dir*dt
	local cnt=self.elapsed//(FRAME2SEC*10)%4
	self.spr=self.spr_ene+self.animdir*16+cnt
end
function Human:dead()
	GAME:add_score(self.score)
	GAME:reduceDiff(30)
	GAME.pl:add_armslv()
	ObjLstA:add(self.pos.x,self.pos.y,Pop2D)
	psfx(6,20,3)
end

-- enedot
EneDot={ base_flag=Flag_Ally }
EneDot.ene_spr=482
setmetatable(EneDot,{__index=Entity})
EneDot.score=300
EneDot.init=function(self)
	self.colli_attr=0
	self.timer=6
	self.captured=false
	self.captime=0
	self.spr=self.ene_spr
	ObjLstA:insert_ene_dot(self)
end
EneDot.upd=function(self,dt)
	if self.captured then
		local t=self.captime*self.captime*self.captime
		self:set_position(lerp(self.pos,GAME.pl.pos,t))
		local ttl=1
		self.captime=min(self.captime+dt/ttl,1)
	elseif self.timer<0 then
		self:del()
		return
	end
	self.spr=self.ene_spr+(self.timer/0.333)%2
	self.timer=self.timer-dt
end
function EneDot:hitcb(dist)
	if dist<self.radius then
		GAME:add_score(self.score)
		GAME:add_multiplier()
		GAME:reduceDiff(0.5)
		psfx(4,5,2)
		self:del()
	else
		self.captured=true
	end
end

-- pop score
Pop2D={elp=0}
setmetatable(Pop2D,{__index=Entity})
Pop2D.spr=484
function Pop2D:init()
	self.dpos=self.pos:Clone()
end
function Pop2D:upd(dt)
	if self.elp >= 1 then
		self:del()
	end
	self.elp=min(self.elp+dt,1)
	self.dpos.y=self.pos.y-self.elp*14
end
function Pop2D:drw(cam)
	local p=self.dpos+cam
	local anim=(self.elp//(FRAME2SEC*4)%2)
	spr(self.spr+anim*2,p.x-8,p.y-4,0,1,0,0,2,1)
end

--game entity table
local ObjLst={}
ObjLst.__index = ObjLst

function CreateEntity(ino,ix,iy,type,args)
	local o=Entity.new(ino,ix,iy)
	local e=setmetatable(o, {__index=type})
	if e.base_flag then
		e.flag = e.flag | e.base_flag
	end
	return e
end

ObjLst.add = function(self,ix,iy,type,args)
	local e = CreateEntity(self.counter,ix,iy,type,args)
	self.counter = self.counter+1
	if e.flag&Flag_bullet ~= 0 then
		insert(self.bullet,#self.bullet+1,e)
	elseif e.flag&Flag_Force ~= 0 then
		insert(self.forces,#self.forces+1,e)
	elseif e.flag&Flag_px ~= 0 then
		self:insert_pxs(e)
	end
	if e.flag&Flag_Verlet ~=0 then insert(self.verlet,#self.verlet+1,e) end
	insert(self.objs,#self.objs+1,e)

	if e.init then e:init(args) end --最後に
	return e
end
function ObjLst:spawn(ix,iy,type,args)
	self.spawn_ttl=self.spawn_ttl+1
	local e=self:add(ix,iy,type,args)
	e.flag=e.flag|Flag_Spawned
	return e
end

function ObjLst:insert_pxs(e)
	e.flag = e.flag | Flag_px
	e.sha=self.PX_SHA -- eにも記録
	self.PX_SHA:add(e, e.aabb0.x, e.aabb0.y, e.aabb_size.x, e.aabb_size.y)
	insert(self.pxs,#self.pxs+1,e)
	insert(self.verlet,#self.verlet+1,e)
end
function ObjLst:insert_ene_bullet(e)
	e.sha=self.ENBLT_SHA -- eにも記録
	self.ENBLT_SHA:add(e, e.aabb0.x, e.aabb0.y, e.aabb_size.x, e.aabb_size.y)
	insert(self.ene_bullet,#self.ene_bullet+1,e)
	insert(self.verlet,#self.verlet+1,e)
end
function ObjLst:insert_ene_dot(e)
	e.sha=self.ENDOT_SHA
	self.ENDOT_SHA:add(e, e.aabb0.x, e.aabb0.y, e.aabb_size.x, e.aabb_size.y)
	insert(self.ene_dot,#self.ene_dot+1,e)
end

function del_obj_from_list(lst, func)
	local p=1
	local cnt=0 --削除数
	for _,o in pairs(lst) do
		if o.flag&Flag_del == 0 then
			lst[p]=o
			p=p+1
		else
			cnt=cnt+1
			if func then func(o) end
		end
	end
	for i=1,cnt do
		lst[#lst]=nil
	end
end

ObjLst.upd_del = function(self)
	del_obj_from_list(self.pxs, function(o) self.PX_SHA:remove(o) end)
	del_obj_from_list(self.bullet)
	del_obj_from_list(self.ene_bullet, function(o) self.ENBLT_SHA:remove(o) end)
	del_obj_from_list(self.ene_dot, function(o) self.ENDOT_SHA:remove(o) end)
	del_obj_from_list(self.verlet)
	del_obj_from_list(self.objs)
end

ObjLst.upd_move = function(self,dt)
	for _,o in pairs(self.pxs) do
		o:pre_pro()
	end
	for _,o in pairs(self.verlet) do
		o:pre_pro()
	end
	self.spawn_num=0
	for _,o in pairs(self.objs) do
		o:upd(dt)
		if o.flag&Flag_Spawned then self.spawn_num=self.spawn_num+1 end
	end
end

function intersect_circle_vs_circle(p1,p2)
	local diff = p2.pos-p1.pos
	local sqr_d = diff:SqrMagnitude()
	local d = sqrt(sqr_d)
	local target = p2.radius + p1.radius
	return (d > 0.0 and d < target)
end
function reciprocal_each(p1,p2)
	local diff = p2.pos-p1.pos
	local sqr_d = diff:SqrMagnitude()
	local d = sqrt(sqr_d)
	local target = p2.radius + p1.radius
	if d > 0.0 and d < target then --// d==0: same particle
		local factor = (d-target) / d * 0.5
		local ebounce=BOUNCE
		p1.mov = p1.mov + diff*factor
		p2.mov = p2.mov - diff*factor
		--// preserve impulse
		local inv_sqr_d = 1/sqr_d
		local f1 = ebounce * Vec2.Dot(p1.vel,diff) * inv_sqr_d --mass:1
		local f2 = ebounce * Vec2.Dot(p2.vel,diff) * inv_sqr_d
		local f1f2=f1-f2
		--p1
		p1.mov_old = p1.mov_old + diff*f1f2 * p1.inv_mass
		p1.hit_mask = p1.hit_mask | p2.colli_attr
		--p2
		p2.mov_old = p2.mov_old - diff*f1f2 * p2.inv_mass
		p2.hit_mask = p2.hit_mask | p1.colli_attr
	end
end
function blt_vs_ene(o,b)
	if o.flag & (Flag_player|Flag_Ally|Flag_del) ~= 0 then return end --player,delは除く
	if intersect_circle_vs_circle(o,b) then
		o:sub_health(b)
		b:del()
	end
end
function force_vs_ene(o,f)
	if o.flag & (Flag_player|Flag_Ally|Flag_del) ~= 0 then return end --player,delは除く
	--intersect_circle_vs_circle
	local diff = o.pos-f.pos
	local d = diff:Magnitude()
	local target = f.radius + o.radius
	if (d < target) then
		f:hitcb(o,diff,d)
	end
end
ObjLst.upd_reciprocal = function(self)
	local sha = self.PX_SHA
	--bullet vs enemy
	for _,b in pairs(self.bullet) do
		sha:each(b.aabb0.x, b.aabb0.y, b.radius*2, b.radius*2,
			function(o) blt_vs_ene(o,b) end)
	end
	--bullet vs ene_bullet
	for _,b in pairs(self.bullet) do
		self.ENBLT_SHA:each(b.aabb0.x, b.aabb0.y, b.radius*2, b.radius*2,
			function(o) blt_vs_ene(o,b) end)
	end
	--force vs enemy
	for _,f in pairs(self.forces) do
		sha:each(f.aabb0.x, f.aabb0.y, f.radius*2, f.radius*2,
			function(o) force_vs_ene(o,f) end)
	end
	--force vs ene_bullet
	for _,f in pairs(self.forces) do
		self.ENBLT_SHA:each(f.aabb0.x, f.aabb0.y, f.radius*2, f.radius*2,
			function(o) force_vs_ene(o,f) end)
	end
	--clear forces
	local n=#self.forces
	for i=1,n do self.forces[i]=nil end

	--player vs ene_bullet
	local pl=GAME.pl
	if pl and pl.flag & Flag_Invincible == 0 then
		self.ENBLT_SHA:each(pl.aabb0.x, pl.aabb0.y, pl.radius*2, pl.radius*2,
		function(o)
			if o.flag & (Flag_del) ~= 0 then return end --delは除く
			if intersect_circle_vs_circle(o,pl) then
				pl.hit_mask=pl.hit_mask|o.colli_attr
				o.hit_mask=o.hit_mask|pl.colli_attr
			end
		end)
	end
	--player vs enedot
	if pl then
		local capr=pl.capradius
		self.ENDOT_SHA:each(pl.pos.x-capr, pl.pos.y-capr, capr*2, capr*2,
		function(o)
			if o.flag & Flag_del ~= 0 then return end --delは除く
			--intersect_circle_vs_circle
			local diff = o.pos-pl.pos
			local d = diff:Magnitude()
			local target = pl.capradius + o.radius
			if (d < target) then
				o:hitcb(d-pl.radius)
			end
		end)
	end
	--obj同士
	for _,obj in pairs(self.pxs) do
		sha:each(obj, function(o)
			if obj.no > o.no and (o.flag & Flag_Invincible)==0 then
				reciprocal_each(obj,o)
			end
		end)
	end
end

ObjLst.upd_colliders = function(self,lst,func)
	--inner circel
	local lvradius = GAME.LvRadius
	local he=GAME.LvRadDr2-8
	for _,obj in pairs(lst) do
		local diff = -obj:get_estimate_pos()
		if abs(diff.x)>=he or abs(diff.y)>=he then
			local len = diff:Magnitude()
			local d = len - lvradius + obj.radius
			if d > EPSILON then
				local dir = diff / len
				if func and func(obj,dir) then
				else
					obj:repulse(dir, -d)
				end
			end
		end
	end
end

ObjLst.upd_verlet = function(self,dt)
	local prev_inv_dt=1/self.prev_dt
	local damping=0.4 --なし:1
	local decel = math.pow(abs(damping), dt);
	for _,o in pairs(self.verlet) do
		o:do_verlet(dt,prev_inv_dt,decel)
	end
end

ObjLst.update = function(self,dt)
	self:upd_move(dt)
	self:upd_del()
	self:upd_reciprocal()
	self:upd_colliders(self.pxs)
	self:upd_colliders(self.bullet,function(o) o:del() return true end)
	self:upd_colliders(self.ene_bullet,function(o,dir) o:hit_wall(dir) return false end)
	self:upd_verlet(dt)
	self.prev_dt=dt
end

ObjLst.draw = function(self,cam)
	for i=#self.objs, 1, -1 do
		self.objs[i]:drw(cam)
	end
	-- for _,o in pairs(self.objs) do
	-- 	o:drw(cam)
	-- end
	if _DEBUG then
		local memuse=collectgarbage("count")
		print_dbg(string.format("o:%3d,p:%3d,eb:%3d,ptc:%3d",#self.objs,#self.pxs,#self.ene_bullet,PtclLst:count()),0,8)
		print_dbg(string.format("mem:%6.1fKB,inp:%d[%d]",memuse,Input:log_num(),Input.pos),0,8+6)
		print_dbg(string.format("SHA PX:%3d,BLT:%3d,DOT:%3d",self.PX_SHA.numentities,self.ENBLT_SHA.numentities,self.ENDOT_SHA.numentities),0,8+6*2)
	end
end

ObjLst.cnt = function(self)
	return #self.objs
end
function ObjLst:get_spawn_ttl()
	return self.spawn_ttl
end
function ObjLst:get_spawn_num()
	return self.spawn_num
end

ObjLst.new = function()
	return setmetatable({
		counter = 0,
		objs = {},
		pxs = {}, --physics
		bullet = {}, --bullet
		ene_bullet = {}, --enemy bullet
		ene_dot = {}, --enedot
		forces={}, --forces
		verlet={},
		PX_SHA=shash.new(10),
		ENBLT_SHA=shash.new(10),
		ENDOT_SHA=shash.new(10),
		prev_dt=1/60,
		spawn_ttl=0,
		spawn_num=0, --num of enemies
	},ObjLst)
end
ObjLstA=ObjLst.new()

------------------------------------------
local Spawner={}
local Spawner_dt=0
Spawner.__index=Spawner

function wait_for_second(sec,func)
	while sec > 0 do
		yield(1)
		if(func)then func() end
		sec = sec - Spawner_dt
	end
end
function matrix_roty(deg)
	-- identity matrix with size 4x4
	local m = matrix (3,"I")
	local rad = math.rad(deg)
	local c = math.cos(rad)
	local s = math.sin(rad)
	m[1][1] = c
	m[1][2] = -s
	m[2][1] = s
	m[2][2] = c
	return m
end

Spawner.test_co = function(self,args)
	while true do
		if ObjLstA:cnt()<10 then
			ObjLstA:spawn(random(-170,170),random(-170,170),args.t)
		end
		wait_for_second(0.5)
	end
end
function random_circle(num,st_r,ed_r,func)
	local r0=st_r/ed_r
	for i=1,num do
		local r = sqrt(randomf(r0,1))*ed_r
		local theta = randomf(-math.pi, math.pi)
		func(r*math.cos(theta),r*math.sin(theta))
	end
end
Spawner.random_co = function(self,args)
	--str:start_radius
	--edr:end_radius
	--num:数
	local r_max=GAME.LvRadius-8
	local st_r=args.str or 0
	local ed_r=args.edr or r_max
	ed_r = math.min(ed_r, r_max)
	local end_wait=args.end_wait or 0
	random_circle(args.num,st_r,ed_r,function(x,y)
 		ObjLstA:spawn(x,y,args.t)
	end)
	wait_for_second(end_wait)
end
Spawner.spiral_co = function(self,args)
	local radius = 160
	for i=0, 110 do
		radius = radius - 1.5
		local m = matrix_roty(15*i)
		local v = matrix { {-radius},{2},{1}, }
		local pos = m*v
		ObjLstA:spawn(pos[1][1],pos[2][1],args.t)
		wait_for_second(0.06)
	end
end
Spawner.circle_co = function(self,args)
	--radius:160まで
	local end_wait=args.end_wait or 0
	local radius = args.radius--60  
	local num = args.num
	local deg_step=360/num
	local dm=args.dirt or 1
	for i=0,num-1 do
		local rad=math.rad(deg_step*i)
		local c=cos(rad)
		local s=sin(rad)
		local m=dm==0 and i%2*2-1 or dm
		ObjLstA:spawn(c*radius,s*radius,args.t,{dir=Vec2.new(c*m,s*m)})
	end
	wait_for_second(end_wait)
end
Spawner.line_co = function(self,args)
	-- rot:rotation_y
	-- step_wait:1体あたりの待ち,default:0
	-- end_wait:全登録あとの待ち
	-- stid:開始のid, 0,1,-8
	local m = matrix_roty(args.rot)
	local stid=args.stid or 1
	local end_wait=args.end_wait or 0
	local step_wait=args.step_wait or 0
	for i=stid, 8 do
		local v = matrix{{i*20},{0},{1}}
		local pos = m*v
		ObjLstA:spawn(pos[1][1],pos[2][1],args.t,args.args)
		wait_for_second(step_wait)
	end
	wait_for_second(end_wait)
end
Spawner.cross_co = function(self,args)
	local rot = args.rot or random(0,1)*45
	local lst={
		{ Spawner.line_co, { t=args.t, rot=rot, step_wait=0, stid=-8 }, 0},
		{ Spawner.line_co, { t=args.t, rot=rot+90, step_wait=0 }, 0},
		{ Spawner.line_co, { t=args.t, rot=rot-90, step_wait=0 }, 5},
	}
	self:run_lst(lst)
end
Spawner.line_intercept_co = function(self,args)
	local end_wait=args.end_wait or 0
	local m = matrix_roty(args.rot)
	local y = args.y
	local width=9
	local r=GAME.LvRadius-8
	local len=sqrt(r*r-y*y)
	local num=len//width
	local ene_args=table.clone(args.args)
	local dir = m*matrix{{ene_args.dir.x},{ene_args.dir.y},{1}}
	ene_args.dir.x,ene_args.dir.y=dir[1][1],dir[2][1]
	for i=-num, num do
		local v = matrix{{i*width},{y},{1}}
		local pos = m*v
		ObjLstA:spawn(pos[1][1],pos[2][1],args.t,ene_args)
	end
	wait_for_second(end_wait)
end
function Spawner:line_onr_co(args)
	local m=matrix_roty(args.rot)
	local w=9
	for i=args.sti,args.edi do
		local v = matrix{{i*w},{0},{1}}
		local pos=m*v
		ObjLstA:spawn(pos[1][1],pos[2][1],args.t,{rdir=args.rdir})
	end
end
Spawner.square_co = function(self,args)
	--y:切片
	--end_wait:終わり待ち
	local ud=1+random(0,1)*(-2)
	for i=0,3 do
		self:runco(Spawner.line_intercept_co, { t=args.t, args={dir=Vec2.new(0,ud)}, y=args.y, rot=90*i+args.rot, end_wait=args.end_wait })
	end
end
function Spawner:radial_co(args)
	local num=16
	for i=0,num-1 do
		self:runco(Spawner.line_onr_co,{t=args.t,rot=360/num*i,sti=14,edi=18,rdir=-1+i%2*2})
	end
	wait_for_second(args.end_wait or 0)
end
Spawner.chase_co=function(self,args)
	--num:数
	--spd:追尾速
	local p=Vec2.new(0,0)
	local dir=Vec2.new()
	for i=1,args.num do
		wait_for_second(args.step_wait,function()
			dir=GAME.pl.pos-p
			dir:SetNormalize()
			p:Add(dir*args.spd*Spawner_dt)
		end)
		ObjLstA:spawn(p.x,p.y,args.t,{dir=dir})
	end
end

function Spawner:run_lst(lst)
	for i=1,#lst do
		local c=lst[i]
		self:runco(c[1],c[2])
		wait_for_second(c[3])
	end
end
function Spawner:registration()
	local tbl = {
---[[
		{ Spawner.spiral_co, { t=EneGrunt } },
		{ Spawner.spiral_co, { t=EneSnake } },
		{ Spawner.cross_co, { t=EneGrunt } },
		{ Spawner.cross_co, { t=EneSnake } },
		{ Spawner.circle_co, { t=EneGrunt, radius=120,end_wait=4,num=40 } },
		{ Spawner.circle_co, { t=EneSnake, radius=120,end_wait=4,num=40 } },
		{ Spawner.circle_co, { t=EneArrow, radius=85,end_wait=4,num=59,dirt=random(0,2)-1 } },
		{ Spawner.random_co, { t=EneGrunt, str=60, edr=160, num=75, end_wait=4 }, },
		{ Spawner.random_co, { t=EneGrunt, edr=60, num=25, end_wait=4 }, },
		{ Spawner.random_co, { t=EneSnake, str=60, edr=160, num=75, end_wait=4 }, },
		{ Spawner.random_co, { t=EneSnake, edr=60, num=25, end_wait=4 }, },
		{ Spawner.random_co, { t=EneHulk, str=80, edr=160, num=10, end_wait=4 }, },
		{ Spawner.square_co, { t=EneArrow, y=115, rot=random(0,1)*45, end_wait=4 }, },
		{ Spawner.square_co, { t=EneArrow2, y=115, rot=random(0,1)*45, end_wait=4 }, },
		{ Spawner.chase_co, { t=EneGrunt, num=50, step_wait=0.4, spd=35 } },
		{ Spawner.chase_co, { t=EneSnake, num=50, step_wait=0.4, spd=35 } },
		{ Spawner.chase_co, { t=EneArrow, num=50, step_wait=0.4, spd=35 } },
		{ Spawner.radial_co, {t=EneSphe,end_wait=4} },
--]]
		--{ Spawner.line_intercept_co, { t=EneArrow, args={dir=Vec2.new(0,-1)}, y=100, rot=90, end_wait=10 } },
		--{ Spawner.line_co, { t=EneGrunt, rot=-45, step_wait=0.2, end_wait=4 } },
	}
	if GAME.boss then return end --boss中
	if ObjLstA:get_spawn_num() >= 200 then return end
	if self:num()<1 and #tbl>0 then
		local lot = random(1,#tbl)
		local c=tbl[lot]
		self:runco(c[1],c[2])
	end
	if ObjLstA:get_spawn_ttl() >= self.boss_step*2000 or dbg_key(KEY.B)then
		ObjLstA:spawn(0,-50,Boss)
		self.boss_step=self.boss_step+1
	end
	if ObjLstA:get_spawn_ttl() >= self.human_step*700 or dbg_key(KEY.H)then
		self:runco(Spawner.random_co, {t=Human,num=3})
		self.human_step=self.human_step+1
	end
end

function Spawner:init()
	--self:runco(Spawner.test_co, {t=EneSphe})
	--ObjLstA:spawn(0,0,EneArrow,{dir=Vec2.new(1,0)})
end
function Spawner:exec(dt)
	self:registration()

	Spawner_dt=dt
	for _,val in pairs(self.lst) do
		_,val.resume_flag = coroutine.resume(val.co,self,val.args)
	end
	--終了分を削除
	for i=#self.lst,1,-1 do
		local flg=self.lst[i].resume_flag
		if flg ~= 1 then -- 1(resume中)でないと終了
			if flg ~= nil then
				assert(false,flg)
			end
			remove(self.lst, i)
		end
	end
end

function Spawner:create_co(func,args)
	return {
		co=coroutine.create(func),
		args=args,
		resume_flag=1,
	}
end
function Spawner:runco(func,args)
	insert(self.lst,#self.lst+1,Spawner:create_co(func,args))
end
function Spawner:num()
	return #self.lst
end

Spawner.new = function()
	local inst = setmetatable({
		lst={},
		human_step=1,
		boss_step=1,
	},Spawner)
	return inst
end

--mode
local Mode = {}
Mode.__index=Mode
function Mode:dest()
	local work=self.cur_work
	if work then work:dest() end
end
function Mode:init()
	local work=self.cur_work
	if work then work:init() end
end
function Mode:ctrl(dt)
	local work=self.cur_work
	if work then return work:ctrl(dt) end
	return false
end
function Mode:update(dt)
	if self.req_work then
		self:dest()
		self.cur_work=self.req_work
		self.req_work=nil
		self:init()
	end
	return self:ctrl(dt)
end
function Mode:update_post()
	local work=self.cur_work
	if work and work.ctrl_post then work:ctrl_post() end
end
function Mode:draw0()
	local work=self.cur_work
	if work and work.draw0 then work:draw0() end
end
function Mode:draw1()
	local work=self.cur_work
	if work and work.draw1 then work:draw1() end
end
function Mode:request(req)
	self.req_work=req
end
function Mode.base_clr()
	ObjLstA=ObjLst.new()
	PtclLst:clear()
	Camera:reset()
end
MODEM=setmetatable({ cur_work=nil, req_work=nil },Mode)

local mode_game={}
mode_game.__index = mode_game
mode_game.init = function(self)
	--clr
	Mode.base_clr()
	Input:start(self.req_state)
	self.spawner:init()
	--obj
	local ro=ObjLstA:add(100,100,Reticle)
	local po=ObjLstA:add(0,0,Player)
	po.reticle=ro
	self.pl=po
	if _DEBUG then
		self.life=999
	end
end
mode_game.ctrl = function(self,dt)
	local ret=true
	if self.state==self.State_Entry then
		GAME.pl:set_active(self.enttm<0)
		if self.enttm+self.entstrx<0 then
			self.state=self.State_Play
		end
		self.enttm=self.enttm-dt*190
	end
	if self.state==self.State_Play then
		local f=function()
			if self.life<=0 or key(KEY.K) then
				self.state=self.State_Over
				Input:term()
				HISCORE=max(self.score,HISCORE)
				return
			end
			self.spawner:exec(dt)
			self:upd_info(dt)
		end
		f()
	end
	if self.state==self.State_Over then
		if btn_dec() then
			MODEM:request(mode_title.new())
		end
		self.ovelp=self.ovelp+dt
		ret=false
	end 
	return ret
end
function mode_game:ctrl_post()
	Camera:upd(self.pl)
end
mode_game.dest = function(self)
	Input:term()
	Mode.base_clr() --clr
end
function mode_game:draw0()
	-- local ccx,ccy,ofsx,ofsy=Camera:calc_cc(0.125)
	-- map(ccx+60,ccy,31,18,ofsx,ofsy)
	local ccx,ccy,ofsx,ofsy=Camera:calc_cc(1)
	map(ccx,ccy,31,18,ofsx,ofsy)--,0)
	-- print(string.format("cx:%f,%f",ccx,ccy),0,10*8)
	-- print(string.format("of:%f,%f",ofsx,ofsy),0,11*8)
	local ct=Camera.trs
	circb(ct.x,ct.y,self.LvRadius,15)
end
mode_game.draw1 = function(self)
	if self.boss then self.boss:drw_info() end
	local c=self.multiplier<=1.0 and 2 or math.floor(self.multime/self.multimeLimit*3)+1
	c=clamp(c,1,3)
	local col={6,15,11}
	print(string.format("%5.2fx",self.multiplier),180+6*2,8,col[c],true)
	print(string.format("% 10d",self.score),180-6,0,15,true)
	for i=1,math.min(self.life-1,10) do
		spr(481,i*8,0,0)
	end
	if self.state==self.State_Entry then
		local y,merg=48,2
		rect(0,y-merg,SCR_WIDTH,5+merg*2,2)
		local str="SAVE THE LAST HUMANS"
		local x=self.enttm
		self.entstrx=print(str,x,y,11)
	elseif self.state==self.State_Over then
		local r=self.ovelp//(FRAME2SEC*26)
		if r%2==0 then
			local gov="GAME OVER"
			local y=(128-8)//2
			local x=print_hcenter(gov,-100)
			print(gov,x+1,y+1,3)
			print(gov,x-1,y-1,3)
			print(gov,x,y,15)
		end
	end
	if _DEBUG then
		print_dbg(string.format("s:%d,spw:%d,dif:%f(%6.1f)",self.spawner:num(),ObjLstA:get_spawn_ttl(),self.difficulty,self.diffsub),0,8+6*3)
		--print_dbg(Camera.center,0,8+6*3)
	end
end
mode_game.new = function(in_req_state)
	in_req_state=in_req_state or Input.StateLog
	return setmetatable({
		State_Entry=0,
		State_Play=1,
		State_Over=2,
		state=0, ovelp=0,
		LvRadius=170,LvRadDr2=170/1.4142,
		pl=nil,
		boss=nil,
		score=0,
		multiplier=1.0,
		multimeLimit=6,--6sec
		multime=0,
		life=3,
		difficulty=0,ticcnt=0,diffsub=500,
		spawner=Spawner.new(),
		enttm=SCR_WIDTH+10,
		entstrx=50,
		req_state=in_req_state,
	},mode_game)
end
function mode_game:add_score(v)
	self.score = self.score + math.floor(v*self.multiplier)
end
function mode_game:add_multiplier()
	self.multiplier=min(self.multiplier+0.01,15)
	self.multime=self.multimeLimit
end
function mode_game:reset_multiplier()
	self.multiplier=1.0
end
function mode_game:decriment_life()
	self.life = math.max(self.life - 1,0)
	return self.life
end
function mode_game:upd_info(dt)
	if self.multime<=0 then
		self:reset_multiplier()
	end
	self.multime=max(self.multime-dt,0)
	if self.ticcnt%30==0 then
		self.difficulty=min(max(ObjLstA:get_spawn_ttl()-self.diffsub,0)/2000,1)
	end
	self.ticcnt=self.ticcnt+1
end
function mode_game:getDifV(a,b)
	return lerp(a,b,self.difficulty)
end
function mode_game:reduceDiff(v)
	self.diffsub=self.diffsub+v
end

mode_title={}
mode_title.__index=mode_title
function mode_title:init()
	local w,h=SCR_WIDTH,SCR_HEIGHT
	for i=1,2000 do
		self.p[i]={x=random(w)-h,y=random(w)-h,z=random(w)}
	end
end
function mode_title:setdec()
	self.decide=true
	self.decide_time=1
	psfx(7,30,0)
end
function mode_title:ctrl(dt)
	self.elp=self.elp+dt
	if self.decide then
		if self.decide_time<0 then
			GAME = mode_game.new(self.decide_type)
			MODEM:request(GAME)
		end
		self.decide_time = self.decide_time - dt
	elseif btn_dec() then
		self.decide_type=CURSOR==0 and Input.StateLog or Input.StateTrace
		self:setdec()
	elseif btn(0) then CURSOR=0
	elseif btn(1) and Input:exists_log() then CURSOR=1
	end
	return true
end
function mode_title:dest()
end
function mode_title:draw0()
	local w,h=SCR_WIDTH,SCR_HEIGHT
	for _,p in pairs(self.p) do
		if p.z>0 then
			local x=p.x/p.z*w+w/2
			local y=p.y/p.z*w+h/2
			pix(x,y,10)
		end
		p.z=p.z%w-1
	end
end
function mode_title:draw1()
	local t="Nibiruman:2080"
	local y=36
	local x=print_hcenter(t,y,12,false,2)
	print(t,x-1,y,12,false,2)
	print(t,x,y-1,12,false,2)
	local dur=self.decide and 6 or 26
	local r=self.elp // (FRAME2SEC*dur)
	if r%2==0 then
		print_hcenter("- Press Z to start -",108,15,false,1)
	end
	local rc=random(1,15)
	x=print_hcenter("GAME START",68,CURSOR==0 and rc or 15)
	print("REPLAY",x,80,CURSOR==1 and rc or (Input:exists_log()and 15 or 10))
	print_hcenter("HIGH SCORE",2,6,false,1)
	print_hcenter(string.format("%d",HISCORE),10,15,true,1)
	print(GAME_VER,4,SCR_HEIGHT-16,7,false)
	print("@tasogare66 2020",4,SCR_HEIGHT-8,7,false)
end
mode_title.new = function()
	return setmetatable({
		p={},
		elp=0,
		decide=false,
		decide_time=0,
		decide_type=Input.StateLog,
	},mode_title)
end

GAME = mode_game.new()
HISCORE=50000
CURSOR=0
MODEM:request(mode_title.new())

_DEBUG,_MUTE,dbg_pause=false,false,false
prev_time=time()/1000
frame_time_sum=0
frame_times={}
FPS=0

function TIC()
	-- calc fps
	local ct=time()/1000
	local dt=ct-prev_time
	prev_time=ct
	frame_times[#frame_times+1]=dt
	frame_time_sum=frame_time_sum+dt
	local frame_num=15
	if #frame_times > frame_num then
		frame_time_sum = frame_time_sum-frame_times[1]
		remove(frame_times,1)
	end
	FPS = frame_num / frame_time_sum

	cls(1)
	local dbg_step=false
	if _DEBUG then
		if dbg_pause and keyp(KEY.P) then dbg_step=true end
		if key(KEY.P) then dbg_pause=true elseif key(KEY.O) then dbg_pause=false end
	end
	if (not dbg_pause) or dbg_step then
		--update input
		dt=Input:upd(dt)
		--update mode
		if MODEM:update(dt) then
			--update entity
			ObjLstA:update(dt)
			PtclLst:update(dt)
		end
		MODEM:update_post()
	end
	MODEM:draw0()
	PtclLst:draw(Camera.trs)
	ObjLstA:draw(Camera.trs)
	MODEM:draw1()
	if _DEBUG then print(string.format("%6.3f",FPS), 0, 0) end
end