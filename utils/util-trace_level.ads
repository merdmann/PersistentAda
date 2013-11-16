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

package Util.Trace_Level is

   type Level_Type is new Natural Range 0..99;

   -- global trace levels
   Library_Level     : constant Level_Type := 90;
   Application_Level : constant Level_Type := 1;

end Util.Trace_Level;
