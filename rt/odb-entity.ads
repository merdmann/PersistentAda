-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-entity.ads,v $
--  Version         : $Revision: 1.5 $                                       --
--  Description     : A collection of persistent objects                     --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 30-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/07/19 21:08:36 $                           --
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
--  This package provides entities. Entities do have names and attributes.   --
--  Attributes are pairs of names and references to other objects.           --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;                     use Ada.Streams.Stream_IO;
with Ada.Streams;                               use Ada.Streams;
use  Ada;

with ODB.Persistent;				use ODB.Persistent;
with ODB.Storage_Header;			use ODB.Storage_Header;
use  ODB;

package ODB.Entity is

   -- this type represents the persistent collection.
   type Object is abstract new Persistent.Object with private;

   Unknown_Attribute : exception;

   type Attribute_Array is array( Positive Range <> ) of Unbounded_String;
   type Attribute_Array_Access is access Attribute_Array;

   ---------------------------------------------------------------------------
   -- Description:
   --    Create an entity of the given name.
   -- Preconditions:
   --    None.
   -- Postconditions:
   --    P.1 - The function returns a reference to the named entity. If the
   --          named entity already exisits, the reference will be returned.
   --          Otherwise a new instance will be created.
   -- Exceptions:
   --    None
   -- Notes:
   ---------------------------------------------------------------------------
   function Create(
      Name : in String ) return Reference;
   ---------------------------------------------------------------------------
   -- Description:
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   function Name(
      This : in Reference ) return String ;

   ---------------------------------------------------------------------------
   -- Description:
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   function Attributes(
      This : in Reference ) return Attribute_Array_Access ;

   ---------------------------------------------------------------------------
   -- Description:
   --    Desctroy the entity
   -- Preconditions:
   --    P.1 - The reference point to a collection.
   -- Postconditions:
   --    C.1 - All resources are released
   --    C.2 - The reference to the entity is set to null
   -- Exceptions:
   --    Invalid_Object - P.1 violated
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Destroy(
      This : in out Reference );

   ---------------------------------------------------------------------------
   -- Description:
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   function Attribute(
      This   : in Reference;
      Name   : in String ) return Reference;

   ---------------------------------------------------------------------------
   -- Description:
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Attribute(
      This   : in Reference;
      Name   : in String;
      Value  : in Reference );


private

   type Object_Data_Type ;
   type Object_Data_Access is access all Object_Data_Type;

   ------------
   -- Object --
   ------------
   type Object is abstract new Persistent.Object with record
         Data : Object_Data_Access := null;
      end record;

   procedure Serialize(
      Item   : in out Object;
      Header : in Storage_Header.Handle;
      S      : in Stream_IO.Stream_Access);

   procedure Deserialize(
      Item   : in out Object;
      Header : in Storage_Header.Handle;
      S      : in Stream_IO.Stream_Access);

end ODB.Entity;
