-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-entity.adb,v $
--  Version         : $Revision: 1.5 $                                       --
--  Description     : A relation of persistent objects                       --
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
with Ada.Tags;     				use Ada.Tags;
with Ada.Text_IO;				use Ada.Text_IO;
with Ada.Exceptions;           			use Ada.Exceptions;
use  Ada;
with Unchecked_Deallocation;


with ODB.Classes;				use ODB.Classes;

package body ODB.Entity is

   Version : constant String := "$Id: odb-entity.adb,v 1.5 2003/07/19 21:08:36 merdmann Exp $";
   
   Class_ID   : Natural;
   D_Name     : constant Natural := 1;
   D_Used     : constant Natural := 2;
   D_Pairs    : constant Natural := 3;

   ---------------
   -- Pair_Type --
   ---------------
   type Pair_Type is record
         Name  : Unbounded_String;
	 Value : Reference;
      end record;

   type Pair_Array is array( Natural range <> ) of Pair_Type;
   type Pair_Array_Access is access Pair_Array;

   procedure Free is
      new Unchecked_Deallocation( Pair_Array, Pair_Array_Access);

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type  is record
         Name  : Unbounded_String;
	 Used  : Natural := 0;
	 Pairs : Pair_Array_Access := null;
      end record;

   procedure Free is
      new Unchecked_Deallocation( Object_Data_Type, Object_Data_Access);

   -- this for internal use only.
   type Handle is access all Object;

   ------------
   -- Entity --
   ------------
   function Entity(
      This : in Reference ) return Object_Data_Access is 
      -- Verify, that a correct object reference is passed and return the 
      -- handle 
   begin
      if This = null then
	 Raise_Exception(Invalid_Object'Identity, "null reference" );
      end if;

      if This.all'Tag /= Object'Tag then
	 Raise_Exception(Invalid_Object'Identity, External_Tag(This.all'Tag) );
      end if;

      return Handle( This ).Data;
   end Entity;

   ------------
   -- Create --
   ------------
   function Create( 
      Name   : in String ) return Reference is 
      -- create an entity. If it is already exisiting use the 
      -- existing one.
      Result : Reference := Lookup_Object( Name );
   begin
      if Result  = null then
         declare
            Data   : Object_Data_Access := new Object_Data_Type;
         begin
            Result := new Object;
            Name_Object( Result, Name );
	    Handle( Result ).Data := Data;

	    Data.Pairs := new Pair_Array( 1..20 );
	    Data.Used  := 0;
            Data.Name  := To_Unbounded_String( Name );
         end;
      end if;

      return Result;
   end Create;

   ----------
   -- Name --
   ----------
   function Name(
      This : in Reference ) return String is 
      R    : Object_Data_Access := Entity( This );
   begin
      return To_String( R.Name );
   end Name;

   ----------------
   -- Attributes --
   ----------------
   function Attributes(
      This   : in Reference ) return Attribute_Array_Access  is 
      R      : Object_Data_Access := Entity( This );
      Result : Attribute_Array_Access := null ;
   begin
      if R.Used = 0 then
         return null;
      end if;

      Result := new Attribute_Array( 1..R.Used );
      for i in 1..R.Used loop
         Result(i) := R.Pairs(i).Name;
      end loop;

      return Result;
   end Attributes;

   ---------------
   -- Attribute --
   ---------------
   function Attribute(
      This   : in Reference;
      Name   : in String ) return Reference is 
      -- get 
      R      : Object_Data_Access  := Entity( This );
      Pairs  : Pair_Array_Access renames R.Pairs;
      Result : Reference := null;
   begin
      for i in Pairs'Range loop
         if Pairs(i).Name = Name then
	    return Pairs(i).Value ;
	 end if;
      end loop;
      
      return null;
   end Attribute;

   ---------------
   -- Attribute --
   ---------------
   procedure Attribute(
      This   : in Reference;
      Name   : in String;
      Value  : in Reference ) is 
      -- add a name/value pair
      R      : Object_Data_Access  := Entity( This );
      Pairs  : Pair_Array_Access renames R.Pairs;
      Used   : Natural           renames R.Used;
   begin
      for i in Pairs'Range loop
         if Pairs(i).Name = Name then
	    Pairs(i).Value := Value ;
	 end if;
      end loop;

      Used := Used + 1;
      if not (Used in Pairs'Range) then
         declare
	   Tmp : Pair_Array_Access := new Pair_Array( 1..Used + 20 );
	 begin
	   Tmp( 1..Used-1 ) := Pairs.all;
	   Free( Pairs );
	   Pairs := Tmp;
      	 end;
      end if;

      Pairs(Used).Name  := To_Unbounded_String(Name);
      Pairs(Used).Value := Value;      
   end Attribute;

   -------------
   -- Destroy --
   -------------
   procedure Destroy(
      This : in out Reference ) is 
      R    : Object_Data_Access  := Entity( This );
   begin
      Free( R.Pairs );
      Free( R );
      This := null;
   end Destroy;

   -----------
   -- Write --
   -----------
   procedure Serialize(
      Item   : in out Object;
      Header : in Storage_Header.Handle;
      S      : in Stream_IO.Stream_Access ) is 
      -- write out the contents  of the complete object.
      R      : Object_Data_Access renames Item.Data;
   begin
      String'Output( S, Attribute( Object'Tag, D_Name ) );
      Unbounded_String'Output( S, R.Name );

      String'Output( S, Attribute( Object'Tag, D_Used ) );
      Natural'Output( S, R.Used );

      String'Output( S, Attribute( Object'Tag, D_Pairs ) );
      for i in 1..R.Used loop
         Unbounded_String'Output(S, R.Pairs(I).Name );
	 Reference'Output(S, R.Pairs(I).Value );
      end loop;
      String'Output( S, "END" );
   end Serialize;

   ----------
   -- Read --
   ----------
   procedure Deserialize(
      Item   : in out Object;
      Header : in Storage_Header.Handle;
      S      : in Stream_IO.Stream_Access ) is
      -- Read the entity from the stream
      R      : Object_Data_Access renames Item.Data;
      ID     : Natural := 0;
   begin

      ID := Attribute( Object'Tag, String'Input(S) );
      while ID /= 0 loop
         case ID is 
            when D_Name =>
               R.Name  := Unbounded_String'Input(S);
	    when D_Used =>
               R.Used  := Natural'Input(S);
	    when D_Pairs =>
               R.Pairs := new Pair_Array( 1..R.Used + 20 );

               for i in 1..R.Used loop
                  R.Pairs(i).Name  := Unbounded_String'Input(S);
	          R.Pairs(i).Value := Reference'Input(S);
               end loop;
            when Others =>
	       null;
         end case;

         ID := Attribute( Object'Tag, String'Input(S) );
      end loop;

   end Deserialize;

   -------------
   -- Factory --
   -------------
   function Factory return Persistent.Reference is
      Result : Reference := new Object;
   begin
      Handle( Result ).Data := new Object_Data_Type;
      return Result;
   end;

begin
   Class_Id := Classes.Register_Factory( Object'Tag, Factory'Access );

   Classes.Attribute( Class_Id, "D_Name",  D_Name  );
   Classes.Attribute( Class_Id, "D_Used",  D_Used  );
   Classes.Attribute( Class_Id, "D_Pairs", D_Pairs );
end ODB.Entity;
