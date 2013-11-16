-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-storage-impl-file_index.adb,v $
--  Version         : $Revision: 1.2 $                                       --
--  Description     : Filesystem based raw object index                      --
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
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.IO_Exceptions;				use Ada.IO_Exceptions;
use  Ada;

with ODB.Object_Index;				use ODB.Object_Index;
use  ODB;

with Unchecked_Deallocation;

package body ODB.Storage.Impl.File_Index is
     
   Version : constant String := 
      "$Id: odb-storage-impl-file_index.adb,v 1.2 2003/10/20 04:26:15 merdmann Exp $";

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type is record
         Path : Unbounded_String := Null_Unbounded_String;
      end record;

   ----------
   -- Free --
   ----------
   procedure Free is
      new Unchecked_Deallocation( Object_Data_Type, Object_Data_Access );

   ----------------
   -- Initialize --
   ----------------
   function Initialize return Object_Data_Access is 
      Result : Object_Data_Access := new Object_Data_Type ;
   begin
      return Result;
   end Initialize;
	
   ----------
   -- Path --
   ----------
   procedure Path(
      This : in out Object;
      Name : in String ) is 
      Data : Object_Data_Access renames This.Data;
   begin
      Data.Path :=To_Unbounded_String( Name );
   end Path;

   ----------
   -- Save --
   ----------
   procedure Save(
      This : in out Object) is
      -- save the file index
      Data : Object_Data_Access renames This.Data;

      File : Stream_IO.File_Type ;
      IDX  : Stream_Access; 
   begin
      Create( File => File, 
         Mode => Out_File, 
	 Name => To_String(Data.Path)
      );
      IDX := Stream( File );

      Reset( This );

      while Has_Next_Element( This ) loop

         Next( This );
         String'Output( IDX, Object_Name( This ) );

      end loop;

      Close(File);    
   end Save;

   ----------
   -- Load --
   ----------
   procedure Load(
      This : in out Object ) is 
      -- load the index file 
      Data   : Object_Data_Access renames This.Data;

      File   : Stream_IO.File_Type ;
      IDX    : Stream_Access;
   begin
      Open( File => File, 
         Mode => In_File, 
	 Name => To_String(Data.Path));

      IDX := Stream(File);

      while not End_Of_File( File ) loop
         Object_Name( This, String'Input(IDX) );
      end loop;

      Close(File);

   exception
      when  ADA.IO_EXCEPTIONS.NAME_ERROR =>
      	 null;

      when Error : others =>
         if Is_Open( File ) then
	    Close(File);
	 end if;
         raise;
   end Load;

   -------------
   -- Destroy --
   -------------
   procedure Destroy(
      This : in out Object ) is 
   begin
      Free( This.Data );

      Object_Index.Destroy( This );
   end Destroy;

end ODB.Storage.Impl.File_Index;
