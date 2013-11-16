-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-abstract_object_loader.ads,v $
--  Description     : The abstract object loader                             --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 19-Aug-2002                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/10/25 20:14:11 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2000-2002 Michael Erdmann                                  --
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
--  This package allows to read in an XML document and to read the contents  --
--  of the document sequentially.                                            --
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
with Ada.Streams;                       	use Ada.Streams;
with Ada.Streams.Stream_IO;             	use Ada.Streams.Stream_IO;

with ODB.Storage_Header;			use ODB.Storage_Header;
use  ODB;

package ODB.Abstract_Object_Loader is

   type Object is abstract tagged null record;
   type Handle is access all Object'Class;

   ---------------------------------------------------------------------------
   -- Description: 
   --    Read the named object from the application pool
   -- Preconditions:
   --    C.1 - The stream has to be valid
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Read(
      This  : in out Object;
      Name  : in String ) is abstract;

   ---------------------------------------------------------------------------
   -- Description: 
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   --------------------------------------------------------------------------- 
   function Header(
      This : in Object ) return Storage_Header.Object is abstract;

   ---------------------------------------------------------------------------
   -- Description: 
   --    Return the memory stream for reading the object
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   --------------------------------------------------------------------------- 
   function Stream( 
      This : in Object ) return Stream_Access is abstract;

   ---------------------------------------------------------------------------
   -- Description: 
   --   Return the class of the object
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   --------------------------------------------------------------------------- 
   function InstanceOf(
      This : in Object ) return String is abstract;

   ---------------------------------------------------------------------------
   -- Description: 
   --    Set the storage path. 
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   --------------------------------------------------------------------------- 
   procedure Path( 
      This  : in out Object;
      Value : in String ) is abstract;


   Invalid_Object : exception ;

   ---------------------------------------------------------------------------
   -- Description: 
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   --------------------------------------------------------------------------- 
   procedure Destroy(
      This : in out Object ) is abstract;

private

   Version : constant String := "$Id: odb-abstract_object_loader.ads,v 1.2 2003/10/25 20:14:11 merdmann Exp $";   
   
end ODB.Abstract_Object_Loader;
