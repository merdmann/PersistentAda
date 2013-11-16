-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-abstract_object_writer.ads,v $
--  Version         : $Revision: 1.2 $                                       --
--  Description     : Abstract Object Writer                                 --
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
with Ada.Streams;                       	use Ada.Streams;
with Ada.Streams.Stream_IO;             	use Ada.Streams.Stream_IO;
with ODB.Storage_Header;			use ODB.Storage_Header;
use  ODB;

package ODB.Abstract_Object_Writer is
  
   type Object is abstract tagged null record;
   type Handle is access all Object'Class;
			     
   ---------------------------------------------------------------------------
   -- Description:
   --    Write the named object into the storage media. The name of the 
   --    object will be used later to retrieve the object from the underlying
   --    storage media.
   -- Preconditions:
   -- Postconditions:
   --    None
   -- Exceptions:
   --    None
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Write(
      This   : in out Object;
      Name   : in String;
      Header : in Storage_Header.Object;
      OData  : in Stream_Element_Array ) is abstract;


   ---------------------------------------------------------------------------
   -- Description: 
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   --------------------------------------------------------------------------- 
   procedure Destroy(
      This : in out Object ) is abstract;

   ---------------------------------------------------------------------------
   -- Description: 
   --   Return the class of the object
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   --------------------------------------------------------------------------- ´
   procedure Path( 
      This  : in out Object;
      Value : in String ) is abstract;

private
   Version : constant String := "$Id: odb-abstract_object_writer.ads,v 1.2 2003/10/25 20:14:11 merdmann Exp $";
            	
end ODB.Abstract_Object_Writer;
