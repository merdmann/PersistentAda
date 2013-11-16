pragma Source_Reference( 1,"person.odb");
 with Ada.Text_IO;    use Ada.Text_IO;
  package body Person  is 
D_Name: constant Natural:= 1;
D_Street: constant Natural:= 2;
D_Zip: constant Natural:= 3;

procedure Serialize( 
   Item   : in out Object;
   Header : in out Storage_Header.Object; 
   S      : in Stream_IO.Stream_Access ) is 
begin
   Register_Attribute( Header, D_Name, Write_Offset( S ), Object'Tag );
   Unbounded_String'Output(S, Item.Name);
   Register_Attribute( Header, D_Street, Write_Offset( S ), Object'Tag );
   Unbounded_String'Output(S, Item.Street);
   Register_Attribute( Header, D_Zip, Write_Offset( S ), Object'Tag );
   Positive'Output(S, Item.Zip);
end;

procedure Deserialize( 
   Item   : in out Object;
   Header : in out Storage_Header.Object; 
   S      : in Stream_IO.Stream_Access ) is 
   Field  : String_Array.Handle := Attributes( Header );
begin
   for i in Field'Range loop
   declare
      ID     : Natural := 0;
      Offset : Natural := 0;
      Name   : constant String := To_String( Field(i) );
   begin
      ID := Classes.Attribute( Object'Tag, Name );
      if ID /= 0 then
         Offset := Storage_Header.Lookup_Attribute( Header, Name );
         Read_Offset( S, Offset );
         case ID is 
            when D_Name =>
            Item.Name:=Unbounded_String'Input(S);
            when D_Street =>
            Item.Street:=Unbounded_String'Input(S);
            when D_Zip =>
            Item.Zip:=Positive'Input(S);
            when Others => null ;
         end case;
      end if;
   end;
   end loop ;
exception
   when Storage_Header.Unknown_Attribute => null;
end;

function Factory return Persistent.Reference is
begin
   return new Object;
end;

procedure Register_Object is
  ID : Natural := 0;
begin
   ID := Classes.Register_Factory(Object'Tag, Factory'Access );
   Classes.Attribute( ID, "D_Name",D_Name);
   Classes.Attribute( ID, "D_Street",D_Street);
   Classes.Attribute( ID, "D_Zip",D_Zip);
end;
-- **** End of the generated code ****
     type Handle is access all Person.Object;
     ------------ 
    -- Create -- 
    ------------ 
    function Create(
       Name   : in String;
       Street : in String ) return Reference is
       Result : Reference := Lookup_Object( Name );
       H      : Handle := null;
    begin
       if Result = null then
          Result := new Object;
       end if;
        H := Handle( Result );
        H.Name   := To_Unbounded_String(Name);
       H.Street := To_Unbounded_String(Street);
       H.Zip    := 9999;
        return Result;
    end Create;
     ---------- 
    -- Name -- 
    ---------- 
    procedure Name(
       This  : in Reference;
       Value : in String ) is
       H     : Handle := Handle(This);
    begin
       H.Name := To_Unbounded_String( Value );
    end Name;
     ------------ 
    -- Street -- 
    ------------ 
    procedure Street(
       This  : in Reference;
       Value : in String ) is
       H     : Handle := Handle(This);
    begin
       H.Street := To_Unbounded_String( Value );
    end Street;
     --------- 
    -- Zip -- 
    --------- 
    procedure Zip(
       This  : in Reference;
       Value : in Natural ) is
       H     : Handle := Handle(This);
    begin
       H.Zip := Value;
    end Zip;
      ------------- 
    -- Display -- 
    ------------- 
    procedure Display(
       This : in Reference ) is
       H    : Handle := Handle(This);
    begin
       Put_Line("Name   : " & To_String( H.Name ) );
       Put_Line("Street : " & To_String( H.Street ) );
    end Display;
  begin
    Register_Object;
 end Person;
