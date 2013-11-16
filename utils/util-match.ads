-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/tools/util/util-match.ads,v $
--  Version         : $Revision: 114 $                                       --
--  Description     : Simple string match                                    --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 20-Oct-2006                                            --                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2007-07-15 11:27:23 +0200 (So, 15 Jul 2007) $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2005 Michael Erdmann                                       --
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
-------------------------------------------------------------------------------
package Util.Match is

   ---------------------------------------------------------------------------
   -- Description:
   --    Matches the pattern against the given string.
   -- Preconditions:
   -- Postconditions:
   --    if the string matches in the sense of the given pattern the function
   --    returns true.
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   function Equal(
      Pattern : in String;
      S       : in String ) return Boolean;

end Util.Match;

