-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/apps/odl/main.gpb,v $
--  Description     : Main driver for the ODL Translator of ODB.	     --
--  Author          : Michael Erdmann                                        --
--  Created         : 18.12.2000                                             --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/10/02 19:30:28 $
--  Status          : $State: Exp $
--                                                                           --
--  Copyright (C) 2003 Michael Erdmann                                       --
--                                                                           --
--  ODB is free software;  you can redistribute it  and/or modify it under   --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion. GNADE is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with ODB;  see file COPYING.  If not, write  --
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
--                                                                           --
--  Functional Description                                                   --
--  ======================                                                   --
--  This package contains the main driver of the translator, which means     --
--  the command line is processed and for each file, the ODL translator      --
--  is invoked.                                                              --
--  If an error happens in the ODL translator, the processed file is         --
--  closed and the next file is invoked for processing.                      --
--                                                                           --
--  Contact                                                                  --
--  =======                                                                  --
--  Error reports shall be handled via http://gnade.sourceforge.net          --
--  Features and ideas via: gnade-develop@lists.sourceforge.net              --
--                                                                           --
--  Author contact:                                                          --
--               purl:/net/michael.erdmann                                   --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Text_IO;               	use Ada.Text_IO;
with ODL.Driver;                	use ODL.Driver;
use  ODL;

procedure Main is
   Version : constant String :=
      "$Id: main.gpb,v 1.2 2003/10/02 19:30:28 merdmann Exp $";
begin
   Driver.Main( "Test Version" );
end Main;
