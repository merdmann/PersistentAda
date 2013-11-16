-- ------------------------------------------------------------------------- --
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/gnade/Makefile
--  Description     : Trace helper                                           --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 30-April-2005                                          --
--  Last Modified By: $Author: merdmann $                                    --
--  Last Modified On: $Date: 2007-07-15 11:27:23 +0200 (So, 15 Jul 2007) $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2005-2007 Michael Erdmann                                  --
--                                                                           --
--  ASCL is copyrighted by the persons and institutions enumerated in the     --
--  AUTHORS file. This file is located in the root directory of the          --
--  ASCL distribution.                                                        --
--                                                                           --
--  ASCL is free software;  you can redistribute it  and/or modify it under   --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  ASCL is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with ASCL;  see file COPYING.  If not, write  --
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
--  MA 02111-1307, USA.                                                      --
--                                                                           --
--  As a special exception,  if other files  instantiate  generics from      --
--  ASCL Ada units, or you link ASCL Ada units or libraries with other         --
--  files  to produce an executable, these  units or libraries do not by     --
--  itself cause the resulting  executable  to  be covered  by the  GNU      --
--  General  Public  License.  This exception does not however invalidate    --
--  any other reasons why  the executable file  might be covered by the      --
--  GNU Public License.                                                      --
--                                                                           --
-- ------------------------------------------------------------------------- --
with Util.Trace;				use Util.Trace;

pragma Elaborate_All( Util.Trace );

package body Util.Trace_Helper is

   Version : constant String :=
      "$Id: util-trace_helper.adb 114 2007-07-15 09:27:23Z merdmann $";

   Trace_Info : Module_Trace_Access := new Module_Tracer_Type ;

   Flow_Level         : Level_Type := Flow;
   Informative_Level  : Level_Type := Informative;
   Functional_Level   : Level_Type := Functional;

   ----------------------
   -- Set_Module_Trace --
   ----------------------
   procedure Set_Module_Trace(
      Module : in out Module_Tracer_Type;
      Level  : in Level_Type ) is
      -- set the trace of a module online
   begin
      Module.Local_Trace_Level := Level;

      Informative_Level := Level + 2;
      Flow_Level        := Level + 1;
      Functional_Level  := Level ;

   end Set_Module_Trace;

   -----------
   -- Enter --
   -----------
   procedure Enter( S : in String ) is
   begin
      LOG( Flow_Level, Module & "." & S );
   end Enter;

   -----------
   -- Leave --
   -----------
   procedure Leave( S : in String; Yields : in String := "" ) is
   begin
      if Yields = "" then
         LOG( Flow_Level, Module & "." & S & " finished" );
      else
         LOG( Flow_Level, Module & "." & S &  " finished, yields='" & Yields & "'" );
      end if;
   end Leave;

   -----------
   -- Error --
   -----------
   procedure Error( S : in String ) is
   begin
      LOG( Flow_Level, Module & "." & S & " *** Error ***" );
   end Error;

   ----------
   -- Info --
   ----------
   procedure Info( S : in String ) is
   begin
      LOG( Informative_Level, "    " & S );
   end Info;

   ----------------------
   -- Report_Excpetion --
   ----------------------
   procedure Trace_Exception(
      E      : in Exception_Occurrence;
      Ignore : in Boolean := True ) is
   begin
      LOG( Flow_Level, "Exception *** " & Exception_Name( E ) & ":" & Exception_Message( E ) );
      Trace.Flush;

      if not Ignore then
         Reraise_Occurrence( E );
      end if;
   end Trace_Exception;

begin
   Register( Module, Trace_Info );
end Util.Trace_Helper;
