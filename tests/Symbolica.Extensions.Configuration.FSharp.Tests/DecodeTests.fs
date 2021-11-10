namespace Symbolica.Extensions.Configuration.FSharp

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open global.Xunit

module Decode =
    module Bool =
        [<Property>]
        let ``should be Success value if can be converted to bool`` value =
            test
                <@ value
                   |> string
                   |> Decode.bool
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to bool`` path =
            test
                <@ "string"
                   |> Decode.bool
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.Boolean'." ]
                ) @>

    module Char =
        [<Property>]
        let ``should be Success value if can be converted to char`` value =
            test
                <@ value
                   |> string
                   |> Decode.char
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to char`` path =
            test
                <@ "string"
                   |> Decode.char
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.Char'." ]
                ) @>

    module DateTime =
        [<Property>]
        let ``should be Success value if can be converted to DateTime`` (value: System.DateTime) =
            let value = value.AddMilliseconds(-value.Millisecond |> float)

            let string = value.ToString("s")

            test
                <@ string
                   |> Decode.dateTime
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to DateTime`` path =
            test
                <@ "string"
                   |> Decode.dateTime
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.DateTime'." ]
                ) @>

    module Float =
        [<Property>]
        let ``should be Success value if can be converted to float`` () =
            Prop.forAll
                (Arb.from<float>
                 |> Arb.filter System.Double.IsFinite)
                (fun value ->
                    test
                        <@ value
                           |> string
                           |> Decode.float
                           |> Binder.eval (SectionStub.Empty "") = Success(value) @>)

        [<Property>]
        let ``should be Failure if string can not be converted to float`` path =
            test
                <@ "string"
                   |> Decode.float
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.Double'." ]
                ) @>

    module Int16 =
        [<Property>]
        let ``should be Success value if can be converted to int16`` value =
            test
                <@ value
                   |> string
                   |> Decode.int16
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to int16`` path =
            test
                <@ "string"
                   |> Decode.int16
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.Int16'." ]
                ) @>

    module Int =
        [<Property>]
        let ``should be Success value if can be converted to int`` value =
            test
                <@ value
                   |> string
                   |> Decode.int
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to int`` path =
            test
                <@ "string"
                   |> Decode.int
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.Int32'." ]
                ) @>

    module Int64 =
        [<Property>]
        let ``should be Success value if can be converted to int64`` value =
            test
                <@ value
                   |> string
                   |> Decode.int64
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to int64`` path =
            test
                <@ "string"
                   |> Decode.int64
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.Int64'." ]
                ) @>

    module UInt16 =
        [<Property>]
        let ``should be Success value if can be converted to uint16`` value =
            test
                <@ value
                   |> string
                   |> Decode.uint16
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to uint16`` path =
            test
                <@ "string"
                   |> Decode.uint16
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.UInt16'." ]
                ) @>

    module UInt =
        [<Property>]
        let ``should be Success value if can be converted to uint`` value =
            test
                <@ value
                   |> string
                   |> Decode.uint
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to uint`` path =
            test
                <@ "string"
                   |> Decode.uint
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.UInt32'." ]
                ) @>

    module UInt64 =
        [<Property>]
        let ``should be Success value if can be converted to uint64`` value =
            test
                <@ value
                   |> string
                   |> Decode.uint64
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to uint64`` path =
            test
                <@ "string"
                   |> Decode.uint64
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.UInt64'." ]
                ) @>

    module Uri =
        [<Property>]
        let ``should be Success value if can be converted to absolute uri`` (HostName host) =
            let value = System.Uri($"https://{host}")

            test
                <@ value
                   |> string
                   |> Decode.uri System.UriKind.Absolute
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

    [<Fact>]
    let ``should be Success value if can be converted to relative uri`` =
        let value = System.Uri("/relative/uri", System.UriKind.Relative)

        test
            <@ value
               |> string
               |> Decode.uri System.UriKind.Relative
               |> Binder.eval (SectionStub.Empty "") = Success(value) @>

    [<Property>]
    let ``should be Failure if string can not be converted to uri`` path =
        test
            <@ "string"
               |> Decode.uri System.UriKind.Absolute
               |> Binder.eval (SectionStub.Empty path) = Failure(
                [ $"Could not decode 'string' at path '{path}' as type 'System.Uri'." ]
            ) @>
