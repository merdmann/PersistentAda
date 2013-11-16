-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/tools/util/util-match.adb,v $
--  Version         : $Revision: 114 $                                       --
--  Description     : Simple string match                                    --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 20-Oct-2006                                            --                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2007-07-15 11:27:23 +0200 (So, 15 Jul 2007) $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2007 Michael Erdmann                                       --
--                                                                           --
--  ADB is free software;  you can redistribute it  and/or modify it under   --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  ODB is distributed in the hope that it will be useful, but WITH-  --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with ADB;  see file COPYING.  If not, write  --
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
--  MA 02111-1307, USA.                                                      --
--                                                                           --
--  As a special exception,  if other files  instantiate  generics from this --
--  unit, or you link  this unit with other files  to produce an executable, --
--  this  unit  does not  by itself cause  the resulting  executable  to  be --
--  covered  by the  GNU  General  Public  License.  This exception does not --
--  however invalidate  any other reasons why  the executable file  might be --
--  covered by the  GNU Public License.                                      --
--                                                                           --
--  Functional Description                                                   --
--  ======================                                                   --
--  This package provides very a basic pattern match implementation.         --
--                                                                           --
--  ? means any single character matches                                     --
--  * any number of characters do match                                      --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  Pattern matching is higly simplified.                                    --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
package body Util.Match is

   Version : constant String :=
       "$Id: util-match.adb 114 2007-07-15 09:27:23Z merdmann $" ;

   ------------
   --  Match --
   ------------
   function Equal(
      Pattern : in String;
      S       : in String ) return Boolean is
      J       : Natural := S'First;
      I      : Natural := Pattern'First;
      Result  : Boolean := True;
   begin
      while I in Pattern'Range loop

         if not ( J in S'Range ) and S /= "" then
            Result := False;
            exit;
         end if;

         if Pattern(i) = '*' then
            exit;
         elsif Pattern(i) = '?' then
            J := J + 1;
         else
            Result := Result and ( Pattern(i) = S(j) );
            J := J + 1;
         end if;

         exit when Result = False;

         I := I + 1;
         if not ( I in Pattern'Range ) and ( J in S'Range ) then
            Result := False;
         end if;

      end loop;

      return Result;
   end Equal;


end Util.Match;

