-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-storage-impl-file.adb,v $
--  Version         : $Revision: 1.3 $                                       --
--  Description     : Filebased persistency                                  --
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
--  This package defines the storage strategy. The object data is put into a --
--  xml wrapper                              				     --
--                                                                           --
--                                                                           --
--  objectfile ::= header data		  				     --
--  header     ::= <header> attribute* </header>  (see odb.storage_header)   --
--  attribute  ::= <attribite name= offset= />				     --
--									     --
--  data       ::= <data> hex data </data>				     --
--                          						     --
--  Restrictions                                                             --
--  ============                                                             --
--  R.1 -                                                                    --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Text_IO;				use Ada.Text_IO;
with Ada.Streams.Stream_IO;			use Ada.Streams.Stream_IO;
with Ada.Characters.Latin_1;			use Ada.Characters.Latin_1;
with Ada.Strings;				use Ada.Strings;
with Ada.Strings.Fixed;				use Ada.Strings.Fixed;
with Ada.Tags;					use Ada.Tags;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Exceptions;           			use Ada.Exceptions;
with Ada.IO_Exceptions;				use Ada.IO_Exceptions;
with Ada.Text_IO;
use  Ada;

with ODB.Persistent;				use ODB.Persistent;
with ODB.Abstract_Object_Loader;		use ODB.Abstract_Object_Loader;
with ODB.Abstract_Object_Writer;		use ODB.Abstract_Object_Writer;

with ODB.Storage.Impl.XML_Object_Loader;	use ODB.Storage.Impl.XML_Object_Loader;
with ODB.Storage.Impl.XML_Object_Writer;	use ODB.Storage.Impl.XML_Object_Writer;
with ODB.Storage.Impl.File_Index;		use ODB.Storage.Impl.File_Index;
use  ODB.Storage.Impl;
use  ODB;


package body ODB.Storage.Impl.File is

   Version : constant String := 
       "$Id: odb-storage-impl-file.adb,v 1.3 2003/10/25 20:14:11 merdmann Exp $";

   Max_Object_Size : constant Stream_Element_Offset := 60_000;
   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type is record
         Pool_Path  : Unbounded_String := Null_Unbounded_String;
	 Index_Path : Unbounded_String := Null_Unbounded_String;
      end record;

   ----------------
   -- Initialize --
   ----------------
   function Initialize return Object_Data_Access is 
      Result : Object_Data_Access := new Object_Data_Type;
   begin
      Result.Index_Path := To_Unbounded_String("./pool.idx");
      Result.Pool_Path  := To_Unbounded_String("./pool/");

      return Result;
   end Initialize;

   ---------------
   -- Pool_Path --
   ---------------
   procedure Pool_Path( 
      This  : in out Object;
      Value : String ) is 
      Data  : Object_Data_Access renames This.Data;
   begin
      Data.Pool_Path := To_Unbounded_String( Value );
   end Pool_Path;

   ----------------
   -- Index_Path --
   ----------------
   procedure Index_Path( 
      This  : in out Object;
      Value : String ) is
      Data  : Object_Data_Access renames This.Data;
   begin
      Data.Index_Path := To_Unbounded_String( Value );
   end Index_Path;

   ----------
   -- Save --
   ----------
   procedure Save( 
      This   : in out Object;
      Writer : in out Abstract_Object_Writer.Object'Class ) is 
      -- save the contents of the pool into the given file name
      Data   : Object_Data_Access renames This.Data;
      Index  : File_Index.Object;
   begin
      Path( Writer, To_String( Data.Pool_Path ) );
      Persistent.Index( Index );

      Path( Index, To_String( Data.Index_Path ) );
      File.Save( This, Index, Writer ); 

      File_Index.Destroy( Index );
   end Save;

   ----------
   -- Load --
   ----------
   procedure Load(
      This   : in out Object;
      Loader : in out Abstract_Object_Loader.Object'Class ) is 
      -- load all object 
      Data   : Object_Data_Access renames This.Data;
      Index  : File_Index.Object;
   begin
      Path( Loader, To_String( Data.Pool_Path ) );
      Path( Index, To_String( Data.Index_Path ) );
      
      Load( This, Index, Loader );

      File_Index.Destroy( Index );
   end Load;

   ----------
   -- Load --
   ----------
   procedure Load(
      This   : in out Object ) is
      Data   : Object_Data_Access renames This.Data;
      Loader : XML_Object_Loader.Object; 
   begin
      Load( This, Loader );
   end Load;
		  		  
   ----------
   -- Save --
   ----------
   procedure Save(
      This   : in out Object ) is
      Data   : Object_Data_Access renames This.Data;
      Writer : XML_Object_Writer.Object; 
   begin
      Save( This, Writer );
   end Save;


end ODB.Storage.Impl.File;
