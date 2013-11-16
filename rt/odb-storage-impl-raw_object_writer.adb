-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-storage-impl-raw_object_writer.adb,v $
--  Version         : $Revision: 1.2 $                                       --
--  Description     : ODB Object Writer for Raw Objects                      --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 21-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/10/25 20:14:11 $                           --
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
with Ada.Characters.Latin_1;			use Ada.Characters.Latin_1;
with Ada.Strings;				use Ada.Strings;
with Ada.Strings.Fixed;				use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Text_IO;				use Ada.Text_IO;

with Util.String_Array;				use Util.String_Array;
use  Util;

with ODB.XML;					use ODB.XML;
use  ODB;

with Unchecked_Deallocation;


package body ODB.Storage.Impl.Raw_Object_Writer is

   Version : constant String 
       := "$Id: odb-storage-impl-raw_object_writer.adb,v 1.2 2003/10/25 20:14:11 merdmann Exp $";

   -----------------
   -- Object_Data --
   -----------------
   type Object_Data is record
         Path : Unbounded_String := To_Unbounded_String(".");
      end record;

   ----------------
   -- Initialize --
   ----------------
   function Initialize return Object_Data_Access is 
      Data : Object_Data_Access := new Object_Data;
   begin
      return Data;
   end Initialize;

   ----------
   -- Free --
   ----------
   procedure Free is
      new Unchecked_Deallocation( Object_Data, Object_Data_Access);

   -------------
   -- Destroy --
   -------------
   procedure Destroy(
      This  : in out Object ) is 
   begin
      Free( This.Data );
   end Destroy;

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

   -----------
   -- Write --
   -----------
   procedure Write(
      This   : in out Object;
      Name   : in String;
      Header : in Storage_Header.Object;
      OData  : in Stream_Element_Array ) is 
      -- Write out the named object base on the given data and header
      Data   : Object_Data_Access renames This.Data;

      File    : Stream_IO.File_Type ;
      S       : Stream_Access ;
      Attribs : String_Array.Handle := Attributes( Header );
   begin
      Create( File => File, 
         Mode => Out_File, 
	 Name => To_String( Data.Path ) & "/" & Name
      );
      S := Stream( File );
      String'Output( S, Class_Name( Header ) );
      Natural'Output( S, Attribs'Length );

      for i in Attribs'Range loop
         Unbounded_String'Output( S, Attribs(i));
	 Natural'Output( S, Lookup_Attribute( Header, To_String(Attribs(i))) );
      end loop;
      Free( Attribs );

      Natural'Output( S, OData'Length );
      Stream_Element_Array'Output( S, OData );

      Close( File );
   end Write;
      	
end ODB.Storage.Impl.Raw_Object_Writer;

