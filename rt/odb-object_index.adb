-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-object_index.adb,v $
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
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Text_IO;				use Ada.Text_IO;
use  Ada;

with Util.List;					

package body ODB.Object_Index is

   package Index_List is new Util.List( Item_Type => Unbounded_String );
   use Index_List;
	        
   Version : constant String := "$Id: odb-object_index.adb,v 1.1 2003/10/20 04:26:15 merdmann Exp $";

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type is record
         Items  : Index_List.Handle ;
	 Reader : Index_List.List_Reader_Handle;
      end record;

   ----------------
   -- Initialize --
   ----------------
   function Initialize return Object_Data_Access is 
      Result : Object_Data_Access := new Object_Data_Type ;

      use Index_List;
   begin
      Result.Items   := Index_List.List; 
      Result.Reader  := List_Reader( Result.Items );

      return Result;
   end Initialize;

   -----------------
   -- Object_Name --
   -----------------
   function Object_Name( 
      This : in Object'Class ) return String is
      Data : Object_Data_Access renames This.Data;

      use Index_List;
   begin
      return To_String( Current( Data.Reader ) );
   end Object_Name;

   -----------------
   -- Object_Name --
   -----------------
   procedure Object_Name(
      This : in out Object'Class;
      Name : in String ) is 
      Data : Object_Data_Access renames This.Data;

      use Index_List;
   begin
      Append(Data.Items, To_Unbounded_String(Name) );
   end Object_Name;

   -------------
   -- Destroy --
   -------------
   procedure Destroy(
      This : in out Object'Class) is
      Data : Object_Data_Access renames This.Data;
   begin
      Index_List.Destroy( Data.Items );
   end Destroy;

   ----------
   -- Next --
   ----------
   procedure Next(
      This : in out Object'Class) is 
      Data : Object_Data_Access renames This.Data;
      Item : Unbounded_String;

      use Index_List;
   begin
      Item := Next( Data.Reader );
   end Next;

   ----------------------
   -- Has_Next_Element --
   ----------------------
   function Has_Next_Element(
      This : in Object'Class ) return Boolean is  
      Data : Object_Data_Access renames This.Data;

      use Index_List;
   begin
      return not List_End( Data.Reader );
   end Has_Next_Element;

   -----------
   -- Reset --
   -----------
   procedure Reset(
      This : in out Object'Class ) is 
      Data : Object_Data_Access renames This.Data;

      use Index_List;
   begin
      Destroy( Data.Reader );
      Data.Reader := List_Reader( Data.Items );
   end Reset;

end ODB.Object_Index;
