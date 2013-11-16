-- ------------------------------------------------------------------------- --
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/tools/util/util-trace.adb,v $
--  Description     : Util trace package                                     --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 30-April-2005                                          --
--  Last Modified By: $Author: merdmann $                                    --
--  Last Modified On: $Date: 2007-07-15 11:27:23 +0200 (So, 15 Jul 2007) $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2005-2007 Michael Erdmann                                  --
--                                                                           --
--  ASCL is copyrighted by the persons and institutions enumerated in the   --
--  AUTHORS file. This file is located in the root directory of the          --
--  ASCL distribution.                                                      --
--                                                                           --
--  ASCL is free software;  you can redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  ASCL is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with ASCL; see file COPYING. If not, write  --
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
--  MA 02111-1307, USA.                                                      --
--                                                                           --
--  As a special exception,  if other files  instantiate  generics from      --
--  ASCL Ada units, or you link ASCL Ada units or libraries with other     --
--  files  to produce an executable, these  units or libraries do not by     --
--  itself cause the resulting  executable  to  be covered  by the  GNU      --
--  General  Public  License.  This exception does not however invalidate    --
--  any other reasons why  the executable file  might be covered by the      --
--  GNU Public License.                                                      --
--                                                                           --
-- ------------------------------------------------------------------------- --
with Ada.Text_IO;			use Ada.Text_IO;
with Ada.Task_Identification;		use Ada.Task_Identification;
with Ada.Real_Time;			use Ada.Real_Time;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

with Ada.Task_Attributes;

with Util.Match;			use Util.Match;
--with Util.Bucket;

package body Util.Trace is

   Version : constant String :=
       "$Id: util-trace.adb 114 2007-07-15 09:27:23Z merdmann $";

   Path         : Unbounded_String := Null_Unbounded_String;
   Global_Level : Level_Type := 0;

   ----------------------------
   -- Module_Descriptor_Type --
   ----------------------------
   type Module_Descriptor_Type is record
         Name        : Unbounded_String    := Null_Unbounded_String;
         Module_Data : Module_Trace_Access := null;
      end record;

   Modules : array( 1..1000 ) of Module_Descriptor_Type ;
   Next    : Natural := Modules'First;

   --------------------------
   -- Trace_Attribute_Type --
   --------------------------
   type Trace_Attribute_Type is record
         Level : Level_Type := 0;
	 File  : File_Type;
      end record;

   type Trace_Attribute_Access is access all Trace_Attribute_Type;

   Null_Trace_Attribute : constant Trace_Attribute_Access := null;

   package Trace_Attributes is new Ada.Task_Attributes(
        Attribute => Trace_Attribute_Access,
      	Initial_Value => Null_Trace_Attribute );
   use Trace_Attributes;

   -----------------
   -- Start_Trace --
   -----------------
   procedure Start_Trace(
      Trace_File : in String := "";
      Level      : in Level_Type := Application_Level;
      Parent     : in Task_ID    := Null_Task_ID ) is
      Handle     : Trace_Attribute_Access := Value;
   begin
      if Handle = null then
         if Parent = Null_Task_ID then
            Handle := new Trace_Attribute_Type ;
         else
            Handle := Value( Parent );
         end if;
         Set_Value( Handle );
      end if;

      if Trace_File /= "" then

         Create(
            File => Handle.File,
            Name => To_String( Path ) & Trace_File & "-" & Image( Current_Task ) & ".log" ,
	    Mode => Out_File );

         Handle.Level := Level;
         Put_Line( Handle.File, "Trace started with level" & Level_Type'Image( Level ) );
      end if;

   end Start_Trace;

   -----------------
   -- Trace_Level --
   -----------------
   function Trace_Level return Level_Type is
      Handle : Trace_Attribute_Access := Value;
   begin
      return Handle.level;
   end Trace_Level;

   -----------------
   -- Trace_Level --
   -----------------
   procedure Trace_Level(
      Level : in Level_Type ) is
   begin
      Global_Level := Level;
      LOG( 0, "Trace_Level set to " & Level_Type'Image( Global_Level ) );
   end Trace_Level;

   -----------
   -- Flush --
   -----------
   procedure Flush is
      Handle : Trace_Attribute_Access := Value;
   begin
      if Handle /= null then
         Flush( Handle.File );
      end if;
   end Flush;

   -----------
   -- Trace --
   -----------
   procedure Log(
      Level	 : in Level_Type := Trace_Level;
      Text       : in String ) is
      Handle     : Trace_Attribute_Access := Value;

      function Format_Level return String is
         Result : String( 1..5 ) := (others => ' ');
	 L      : String := Level_Type'Image(Level);
	 J      : Natural := L'Last;
      begin
         for i in reverse Result'range loop
	    Result( I ) := L( J );
	    J := J - 1;
	    exit when j = 0;
	 end loop;

         return Result;
      end Format_Level;

      function Indent return String is
         Result    : String( 1..80 ) := ( others => ' ' );
	 Indention : Natural := 1 + Natural(Level) / 10 ;
      begin
         return Result( 1..Indention );
      end Indent;

   begin
      if Handle /= null then
         if Handle.Level >= Level or Global_Level >= Level then
	    declare
	       SC : Seconds_Count;
	       TS : Time_Span;
	    begin
	       Split( Clock, SC, TS );
               Put_Line( Handle.File, Image( Current_Task ) & "." &
	         Seconds_Count'Image(SC) & "." & Duration'Image( To_Duration(TS) ) &
	         " " & Format_Level & " : " & Indent & Text );
	    end;
         end if;
      end if;
   end Log;

   ----------
   -- Stop --
   ----------
   procedure Stop_Trace is
      Handle : Trace_Attribute_Access := Value;
   begin
      if Handle /= null then
         Put_Line(Handle.File, "Trace stoped");
         Put_Line(Handle.File, "" );
         Flush(Handle.File);
         if Is_Open( Handle.File ) then
            Close( Handle.File );
         end if;
      end if;
   end Stop_Trace;

   --------------
   -- Location --
   --------------
   procedure Directory( Trace_Path : in String ) is
   begin
      Path := To_Unbounded_String( Trace_Path & "/" );
   end Directory;

   ----------------------
   -- Set_Module_Trace --
   ----------------------
   procedure Set_Module_Trace(
      Module : in out Module_Trace_Type;
      Level  : in Level_Type )  is
   begin
      null;
   end Set_Module_Trace;

   --------------
   -- Register --
   --------------
   procedure Register(
      S      : in String;
      Tracer : in Module_Trace_Access ) is
      -- this procedure is called by each module to register its module
   begin
      if Next in Modules'Range then
         Modules(Next).Name := To_Unbounded_String(S);
         Modules(Next).Module_Data := Tracer;

         Next := Next + 1;
      end if;
   end Register;

   -----------------
   -- Trace_Level --
   -----------------
   procedure Trace_Level(
      Module : in String;
      Level  : in Level_Type ) is
      -- set the default trace level for the given modules
   begin
      for i in Modules'Range loop
         exit when Modules(I).Name = Null_Unbounded_String;

         if Equal( Pattern => Module, S => To_String(Modules(I).Name) ) then
            Set_Module_Trace( Modules(i).Module_Data.all, Level );
         end if;
      end loop;
   end Trace_Level;

end Util.Trace;
