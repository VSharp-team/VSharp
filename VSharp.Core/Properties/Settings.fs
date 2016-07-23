namespace VSharp.Core

open System.Configuration

type AppSettings() = 
    inherit ApplicationSettingsBase()
    [<UserScopedSettingAttribute()>]
        member this.InfiniteIntegers
            with get() = this.Item("InfiniteIntegers") :?> bool
            and set(value : bool) = this.Item("InfiniteIntegers") <- value

module Properties =
    let Settings = new AppSettings()