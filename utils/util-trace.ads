-- ------------------------------------------------------------------------- --
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/gnade/Makefile
--  Description     : Util base package                                      --
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
--  Public License  distributed with ASCL;  see file COPYING.  If not, write--
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
--  Functional Description                                                   --
--  ======================                                                   --
--  This module provides provides a trace files. The idea is that trace      --
--  trace information are always written by the application but only stored  --
--  in a trace file when the tracefile has been opened.                      --
--                                                                           --
--  Tracefiles are created per task of an application.                       --
--                                                                           --
--  Each trace line created by an application can be assgined to a level.    --
--  The line is shown if the level is less or equal the global trace level.  --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  None                                                                     --
--                                                                           --
-- ------------------------------------------------------------------------- --
with Ada.Task_Identification;			use Ada.Task_Identification;
with Util.Trace_Level;				use Util.Trace_Level;

package Util.Trace is

   ---------------------------------------------------------------------------
   -- Description:
   --    Start a trace session for the current task with a given trace
   --    level.
   -- Preconditions:
   --     None
   -- Postconditions:
   --     P.1 - Tracefile with name of the current thread created
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Start_Trace(
      Trace_File : in String := "";
      Level      : in Level_Type := Application_Level;
      Parent     : in Task_ID := Null_Task_ID);

   ---------------------------------------------------------------------------
   -- Description:
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   function Trace_Level return Level_Type ;

   ---------------------------------------------------------------------------
   -- Description:
   --    Write a trace entry with the given trace level
   -- Preconditions:
   --    None
   -- Postconditions:
   --    P.1 - If a trace session has been created previously by means
   --          of Start_Trace the entry is appended to the file
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Log(
      Level	 : in Level_Type := Trace_Level;
      Text       : in String ) ;

   ---------------------------------------------------------------------------
   -- Description:
   --   Set the directory where the trace files are written.
   -- Preconditions:
   --   None
   -- Postconditions:
   --   P.1 - Directory is changed.
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Directory(
      Trace_Path : in String );

   ---------------------------------------------------------------------------
   -- Description:
   --    Set the global trace level.
   -- Preconditions:
   --    None
   -- Postconditions:
   --    P.1 - All entries written via LOG with a trace level higher then
   --          the trace level given here is written into the trace.
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Trace_Level(
      Level : in Level_Type );

   ---------------------------------------------------------------------------
   -- Description:
   --    Stop the current trace.
   -- Preconditions:
   --    None
   -- Postconditions:
   --    P.1 - The current trace file is closed and no records are written
   --          into the trace file.
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Stop_Trace;

   ---------------------------------------------------------------------------
   -- Description:
   --    Flush the trace
   -- Preconditions:
   --    None
   -- Postconditions:
   --    P.1 - All records are flushed from the write buffer into the file.
   -- Exceptions:
   -- Notes:
   --    This method should be used for example in the context of exception
   --    handlers in order to avoid that trace records are getting lost.
   ---------------------------------------------------------------------------
   procedure Flush;

   -- this type is used to dispatch into the different packages
   type Module_Trace_Type is tagged null record;
   type Module_Trace_Access is access all Module_Trace_Type'Class;

   ---------------------------------------------------------------------------
   -- Description:
   --    This procedure is expected to be overriden in a tracing module. It
   --    is expected set the trace level for the module.
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   --    see Util.Trace_Helper.
   ---------------------------------------------------------------------------
   procedure Set_Module_Trace(
      Module : in out Module_Trace_Type;
      Level  : in Level_Type ) ;

   ---------------------------------------------------------------------------
   -- Description:
   --    Register the named module with the trace system. The trace object
   --    provided by the registered module maintains locally the state
   --    of the trace.
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Register(
          S      : in String;
          Tracer : in Module_Trace_Access  );

   ---------------------------------------------------------------------------
   -- Description:
   --    Set the trace level for a named module
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Trace_Level(
      Module : in String;
      Level  : in Level_Type );

end Util.Trace;
