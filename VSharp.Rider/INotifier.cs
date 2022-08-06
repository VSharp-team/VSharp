using System;

namespace VSharp;

public interface INotifier
{
    String Content(Nillable<Project> project, Nullable<Module> module, String info);
    void Notify(String info, Nullable<Project> project, Nullable<Module> module);
}