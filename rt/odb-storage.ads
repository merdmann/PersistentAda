-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-storage.ads,v $
--  Version         : $Revision: 1.5 $                                       --
--  Description     : Storage Abstraction                                    --
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
--  This package provides the generic interface to the underlying storage    --
--  device.                                                                  --
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
with ODB.Object_Index;				use ODB.Object_Index;
with ODB.Abstract_Object_Loader;		use ODB.Abstract_Object_Loader;
with ODB.Abstract_Object_Writer;		use ODB.Abstract_Object_Writer;

with Util.List;

package ODB.Storage is

   type Object is abstract tagged private;
   type Handle is access all Object'Class;
      
   ---------------------------------------------------------------------------
   -- Description: 
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Load(
      This   : in out Object;
      Index  : in out Object_Index.Object'Class;
      Loader : in out Abstract_Object_Loader.Object'Class ) ;

   ---------------------------------------------------------------------------
   -- Description: 
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Save( 
      This   : in out Object;
      Index  : in out Object_Index.Object'Class;
      Writer : in out Abstract_Object_Writer.Object'Class ) ;

private
   type Object is abstract tagged null record;

end ODB.Storage;
