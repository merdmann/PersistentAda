-- ------------------------------------------------------------------------- --
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/tools/util/util-trace_helper.ads,v $
--  Description     : Trace Helper package                                   --
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
--  sion. Gnade is distributed in the hope that it will be useful, but WITH- --
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
--  Functional Description                                                   --
--  ======================                                                   --
--  This package is a helper for Util.trace package. It provides trace       --
--  functions to visialize the flow of control by providing a procedure      --
--  to be called on entry into a procedure/function and another one for      --
--  exiting.                                                                 --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  None                                                                     --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-- ------------------------------------------------------------------------- --
with Ada.Exceptions;				use Ada.Exceptions;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Util.Trace_Level;				use Util.Trace_Level;
with Util.Trace;				use Util.Trace;

generic
    Module : String ;
    Level  : Level_Type := Application_Level;

package Util.Trace_Helper is

   ---------------------------------------------------------------------------
   -- Description:
   --    The following procedures are providing trace information for entering
   --    leving, error and simple infromational trace information.
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Enter( S : in String );
   procedure Leave( S : in String; Yields : in String := "" );
   procedure Error( S : in String );
   procedure Info( S : in String );

   ---------------------------------------------------------------------------
   -- Description:
   --    This procedure shall be used to catch and trace expections.
   -- Preconditions:
   --    Exception has happend and has been catched by when E:other =>
   -- Postconditions:
   --    Trace information is written on the flow level. If the flag
   --    ignore has been set, the exception is not raised any more.
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Trace_Exception(
      E      : in Exception_Occurrence;
      Ignore : Boolean := True );

   -- the package provide three trace levels relative to the module
   -- trace level.
   Informative       : constant Level_Type := Level + 2;
   Flow              : constant Level_Type := Level + 1;
   Functional        : constant Level_Type := Level    ;

private
   --- #################################################################### ---

   type Module_Tracer_Type is new Trace.Module_Trace_Type with record
         Local_Trace_Level : Level_Type := Informative;
      end record;

   procedure Set_Module_Trace(
      Module : in out Module_Tracer_Type;
      Level  : in Level_Type ) ;

end Util.Trace_Helper;
