-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/test/odl/main.adb,v $
--  Version         : $Revision: 1.3 $                                       --
--  Description     : Definition of a persitent object                       --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 21-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/10/25 20:14:17 $                           --
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
--  R.1 - This package is not designed to be task save                       --
--  R.2 - The object storage cannot be shared between applications.          --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Text_IO;			use Ada.Text_IO;
with ODB.Persistent;			use ODB.Persistent;
with ODB.Storage.Impl.File;		use ODB.Storage.Impl.File;
use  ODB.Storage.Impl;
use  ODB;

with Customer;				use Customer;
with Version;

procedure Main is 
   T : Reference := Null;
   File_Store : File.Object;
begin
   Put_Line( "Basic ODL example, Version " & Version.ID );
   Put_Line( "Copyright (C) 2003; Michael Erdmann (http://gnade.sourceforge.net/)");
   Put_Line( "" );

   File.Pool_Path( File_Store, "./data/");
   File.Index_Path( File_Store, "./data.idx" );

   File.Load(File_Store);

   T := Customer.Create( "Michael_Erdmann", "Torellstrasse 2", 1002 );

   Display(T);

   Put_Line("Saving context");
   File.Save(File_Store);
end Main;

