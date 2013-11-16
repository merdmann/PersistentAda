-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-storage-impl-raw_object_loader.adb,v $
--  Description     : Load object from the an external xml file              --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 19-Aug-2002                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/10/25 20:14:11 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2003 Michael Erdmann                                       --
--                                                                           --
--  XMLS  is free software;  you can redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion. XMLS is distributed in the hope that it will be useful, but WITH-  --
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
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Strings;                       use Ada.Strings;

with Ada.Unchecked_Conversion;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Characters.Latin_1;            use Ada.Characters;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Streams;                       use Ada.Streams;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;

with Sax.Readers;                       use Sax.Readers;
with Sax.Exceptions;                    use Sax.Exceptions;
with Sax.Encodings;                     use Sax.Encodings;
with Sax.Locators;                      use Sax.Locators;
with Sax.Attributes;                    use Sax.Attributes;
with Sax.Models;                        use Sax.Models;

with Unicode.CES;                       use Unicode.CES;
with Unicode;                           use Unicode;
with Unicode.Names.Basic_Latin;         use Unicode.Names.Basic_Latin;
with Input_Sources.File;                use Input_Sources.File;

with Sax.Exceptions;
with Sax.Locators;
with Sax.Readers;
with Sax.Attributes;
with Sax.Models;
with Unicode.CES;

with Unchecked_Deallocation;

with ODB.Memory_Stream;			        use ODB.Memory_Stream;
with ODB.Storage_Header;		        use ODB.Storage_Header;
with ODB.Attribute_Dictionary;		    use ODB.Attribute_Dictionary;
with ODB.XML;				            use ODB.XML;
use  ODB;

package body ODB.Storage.Impl.Raw_Object_Loader is

   Version : constant String :=
       "$Id: odb-storage-impl-raw_object_loader.adb,v 1.2 2003/10/25 20:14:11 merdmann Exp $";

   -------------------
   -- Buffer_Access --
   -------------------
   type Buffer_Access is access Stream_Element_Array;

   -----------------
   -- Object_Data --
   -----------------
   type Object_Data_Type is record
         Attribs    : Attribute_Dictionary.Object( 200 );
	 Name       : Unbounded_String     := Null_Unbounded_String;
	 Next       : Stream_Element_Offset:= 1;
	 OData      : Stream_Access        := null;
	 Header     : Storage_Header.Object;
	 Path       : Unbounded_String     := To_Unbounded_String(".");
      end record;

   ----------------
   -- Initialize --
   ----------------
   function Initialize return Object_Data_Access is
      Data : Object_Data_Access := new Object_Data_Type;
   begin
      return Data;
   end Initialize;

   ----------
   -- Free --
   ----------
   procedure Free is
      new Unchecked_Deallocation( Object_Data_Type, Object_Data_Access);

   ----------
   -- Path --
   ----------
   procedure Path(
      This   : in out Object;
      Value  : in String ) is
      Data   : Object_Data_Access renames This.Data;
   begin
      Data.Path := To_Unbounded_String( Value );
   end Path;

   ----------
   -- Read --
   ----------
   procedure Read(
      This  : in out Object;
      Name  : in String ) is
      -- read a configuration file and build the attribute tree.
      Data  : Object_Data_Access  renames This.Data;
      Input : Stream_IO.File_Type ;
      S     : Stream_Access ;
      Nbr   : Natural := 0;
   begin
      Stream_IO.Open( Name => To_String( Data.Path ) & "/" & Name,
         File => Input,
	 Mode => In_File);

      S := Stream_IO.Stream( Input );

      Class_Name( Data.Header, String'Input( S ) );
      Data.Name := To_Unbounded_String( Class_Name( Data.Header ) );

      Nbr := Natural'Input(S);
      for i in 1..Nbr loop
         declare
	    Field  : constant Unbounded_String := Unbounded_String'Input(S);
	    Offset : constant Natural := Natural'Input(S);
	 begin
	    Register_Attribute( Data.Header, Field, Offset );
	 end;
      end loop;

      Nbr := Natural'Input(S);
      declare
         Buffer : Stream_Element_Array( 1..Stream_Element_Offset(Nbr) );
      begin
         Buffer := Stream_Element_Array'Input( S );
         Data.OData  := Memory_Stream.Stream( Stream_Element_Offset(Nbr) );
         Copy_In( Data.OData, Buffer );
      end;
      Close( Input );
   exception
      when others =>
         Close(Input);
         raise;
   end Read;

   ------------
   -- Header --
   ------------
   function Header(
      This : in Object ) return Storage_Header.Object is
      Data : Object_Data_Access  renames This.Data;
   begin
      return Data.Header;
   end Header;

   ------------
   -- Stream --
   ------------
   function Stream(
      This : in Object ) return Stream_Access is
      Data : Object_Data_Access  renames This.Data;
   begin
      return Data.OData;
   end Stream;

   ----------------
   -- InstanceOf --
   ----------------
   function InstanceOf(
      This : in Object ) return String is
      Data : Object_Data_Access  renames This.Data;
   begin
      return To_String(Data.Name);
   end InstanceOf;

   -------------
   -- Destroy --
   -------------
   procedure Destroy(
      This : in out Object ) is
   begin
      Free( This.Data );
   end Destroy;

end ODB.Storage.Impl.Raw_Object_Loader;
