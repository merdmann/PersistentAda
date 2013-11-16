-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-storage.adb,v $
--  Version         : $Revision: 1.6 $                                       --
--  Description     : This package defines the general storage strategy      --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 30-June-2003                                           --
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
--  This package contains the code to save and restore the data of an single --
--  object. The object contents is written into a memory buffer by calling   --
--  the serialize proceudre provided by the class of the object which has to --
--  be stored.                                                               --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Text_IO;				use Ada.Text_IO;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;                     use Ada.Streams.Stream_IO;
with Ada.Streams;                               use Ada.Streams;
with Ada.Tags;					use Ada.Tags;
with Ada.Exceptions;           			use Ada.Exceptions;
with Ada.IO_Exceptions;				use Ada.IO_Exceptions;
use  Ada;

with ODB.Classes;				use ODB.Classes;
with ODB.Memory_Stream;				use ODB.Memory_Stream;
with ODB.Object_Index;				use ODB.Object_Index;
with ODB.Abstract_Object_Loader;		use ODB.Abstract_Object_Loader;
with ODB.Abstract_Object_Writer;		use ODB.Abstract_Object_Writer;
with ODB.Storage_Header;			use ODB.Storage_Header;
use  ODB;

with Util.String_Array;				use Util.String_Array;
use  Util;

package body ODB.Storage is

   Version : constant String :=
      "$Id: odb-storage.adb,v 1.6 2003/10/25 20:14:11 merdmann Exp $";

   Max_Object_Size : constant Stream_Element_Offset := 60_000;

   type Load_Phase_Type is ( Loading , Resolving );

   ----------
   -- Load --
   ----------
   procedure Load(
      This   : in out Object;
      Index  : in out Object_Index.Object'Class;
      Loader : in out Abstract_Object_Loader.Object'Class ) is
      --
      -- load all objects which are named in the implementation of the
      -- object index by means of the given loader instance. This procedure
      -- is expected to be called by the strategy.
      --
      procedure Load_Instances(
         Phase  : in Load_Phase_Type ) is
	     Header : Storage_Header.Object;
      begin
         Reset( Index );

         while Has_next_Element( Index ) loop
	        Next( Index );

	        declare
               Name   : constant String := Object_Name( Index );
	           Result : Reference       := null;
	           OBS    : Stream_Access ;
	        begin
	           Read( Loader, Name );

               Header := Abstract_Object_Loader.Header( Loader );
	           OBS    := Stream( Loader );

               if Phase = Loading then
                  Result := Classes.Factory(Internal_Tag(InstanceOf(Loader))).all;
	              Name_Object( Result, Name );
               else
                  Result := Lookup_Object( Name );
               end if;

               if Result = null then
                  Raise_Exception(Unresolved_Reference'Identity, Name);
               end if;

               Deserialize( Result.all, Header, Obs );

	           Destroy( OBS );
	           Clear( Header );

	        exception
	           when Error : Others =>
	              Text_IO.Put_Line("Exception while loading object " & Name );
		          Text_IO.Put_Line(Exception_Name(Error) & ", " &
		                Exception_Message( Error ) );
		       raise;
	        end ;
         end loop;
      end Load_Instances;

   begin
      Load( Index );

      Load_Instances( Loading );
      Load_Instances( Resolving );
   end Load;

   ----------
   -- Save --
   ----------
   procedure Save(
      This   : in out Object;
      Index  : in out Object_Index.Object'Class;
      Writer : in out Abstract_Object_Writer.Object'Class ) is
      --
      -- save all objects listed in the index. If the name of an object
      -- is available in the index, it may be assumed, that the object
      -- is persistent.
      --
      OBS    : Stream_Access;
      Header : Storage_Header.Object;
   begin
      OBS  := Memory_Stream.Stream(Max_Object_Size);

      Reset( Index );

      while Has_Next_Element( Index ) loop
	     Next( Index );

         declare
	        Name   : constant String := Object_Name( Index );
            Last   : Stream_Element_Offset := 0;
	        Buffer : Stream_Element_Array( 1..Max_Object_Size );
	        Ref    : Reference := Lookup_Object( Name );
	     begin
            Memory_Stream.Clear( OBS );
            Serialize( Ref.all, Header, OBS );
	        Copy_Out( OBS, Buffer, Last );

            Class_Name( Header, External_Tag(Ref.all'Tag)  );
	        Write( Writer, Name, Header, Buffer( 1..Last ) );

	        Storage_Header.Clear( Header );
	     end ;
      end loop;

      Save( Index );
   end Save;

end ODB.Storage;
