-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-object_index.ads,v $
--  Version         : $Revision: 1.1 $                                       --
--  Description     : ODB Utility root package                               --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 21-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/10/20 04:26:15 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2003 Michael Erdmann                                       --
--                                                                           --
--  ODB is free software;  you can redistribute it  and/or modify it under   --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  ODB is distributed in the hope that it will be useful, but WITH-  --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
package ODB.Object_Index is

    type Object is abstract tagged private;
    type Handle is access all Object'Class;

   ---------------------------------------------------------------------------
   -- Description: 
   --    Return the current object name
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   function Object_Name( 
      This : in Object'Class ) return String ;

   procedure Object_Name(
      This : in out Object'Class;
      Name : in String );

   procedure Destroy(
      This : in out Object'Class );

   procedure Next(
      This : in out Object'Class );

   function Has_Next_Element(
      This : in Object'Class ) return Boolean;

   procedure Reset(
      This : in out Object'Class );

   procedure Save(
      This : in out Object ) is abstract;

   procedure Load(
      This : in out Object ) is abstract;

private
   type Object_Data_Type;
   type Object_Data_Access is access Object_Data_Type;

   function Initialize return Object_Data_Access;

   type Object is abstract tagged record
         Data : Object_Data_Access := Initialize;
      end record;

      	
end ODB.Object_Index;
