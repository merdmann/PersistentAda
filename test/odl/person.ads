pragma Source_Reference( 1,"person.ods");
with Ada.Tags;                    use Ada.Tags;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Streams;                 use Ada.Streams;
with Ada.Streams.Stream_IO;
use  Ada;

with ODB.Persistent;              use ODB.Persistent;
with ODB.Classes;                 use ODB.Classes;
with ODB.Storage_Header;          use ODB.Storage_Header;
with ODB.Memory_Stream;           use ODB.Memory_Stream;
use  ODB;
with Util.String_Array;           use Util.String_Array;
use  Util;

 with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
  package Person  is
     type Object is new Persistent.Object with  private;
     function Create(
       Name   : in String;
       Street : in String ) return Reference;
     procedure Display(
       This : in Reference ) ;
     procedure Name(
       This  : in Reference;
       Value : in String ) ;
     procedure Street(
       This  : in Reference;
       Value : in String ) ;
         procedure Zip(
       This  : in Reference;
       Value : in Natural ) ;
  private
     type Object is new Persistent.Object with  record
           Name    : Unbounded_String ;
    Street  : Unbounded_String ;
    Zip     : Positive Range 1..9999 ;
        end record; 


procedure Serialize(
   Item   : in out Object;
   Header : in out Storage_Header.Object ; 
   S      : in Stream_IO.Stream_Access);
procedure Deserialize(
   Item   : in out Object;
   Header : in out Storage_Header.Object ; 
   S      : in Stream_IO.Stream_Access);
  end Person;
