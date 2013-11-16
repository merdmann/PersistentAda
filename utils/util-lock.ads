-- ------------------------------------------------------------------------- --
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/gnade/Makefile
--  Description     : A simple lock implementation                                      --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 30-April-2005                                          --
--  Last Modified By: $Author: merdmann $                                    --
--  Last Modified On: $Date: 2007-07-15 11:27:23 +0200 (Sun, 15 Jul 2007) $                           --
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
--  sion. ASCL is distributed in the hope that it will be useful, but WITH- --
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
with Ada.Task_Identification;		use Ada.Task_Identification;
with Ada.Finalization;			use Ada.Finalization;

package Util.Lock is

   Not_Lock_Owner : exception;
   Invalid_Lock   : exception;

   -- this is the lock object.
   type Object is new Controlled with private;

   ---------------------------------------------------------------------------
   -- Description:
   --    Claims the lock object.
   -- Preconditions:
   --    none
   -- Postconditions:
   --    In case the lock is already claimed by some body else, the
   --    call will not return until the lock becomes available for the
   --    caller.
   -- Exceptions:
   --    None
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Claim(
     This : in out Object );

   ---------------------------------------------------------------------------
   -- Description:
   --    Release lock
   -- Preconditions:
   --    The lock has been claimed previously.
   -- Postconditions:
   --    The lock is available of other claims.
   -- Exceptions:
   --    None
   -- Notes:
   --    None
   ---------------------------------------------------------------------------
   procedure Release(
     This : in out Object );

   ---------------------------------------------------------------------------
   -- Description:
   --    Returns the task id of the task which is currently holding the
   --    lock.
   -- Preconditions:
   --    None
   -- Postconditions:
   --    Returns the task id of the owner.
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   function Owner(
      This : in Object ) return Task_ID ;

private
   type Lock_Type;
   type Lock_Type_Access is access all Lock_Type;

   type Object is new Controlled with record
         Id   : Natural := 0;
         Lock : Lock_Type_Access := null;
      end record;

   procedure Initialize( This : in out Object );
   procedure Finalize( This : in out Object );

end Util.Lock;
