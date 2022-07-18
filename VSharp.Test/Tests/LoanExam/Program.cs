using System.Reflection;
using System.Text.Json.Serialization;
using FluentValidation.AspNetCore;
using LoanExam;
using Microsoft.AspNetCore.Builder;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using MinimalApi.Endpoint.Extensions;

var builder = WebApplication.CreateBuilder(args);

builder.Services.AddEndpoints();
builder.Services.AddFluentValidation(fv =>
{
    fv.LocalizationEnabled = false;
    fv.RegisterValidatorsFromAssembly(Assembly.GetExecutingAssembly());
});

builder.Services.Configure<Microsoft.AspNetCore.Mvc.JsonOptions>(
    options => options
        .JsonSerializerOptions
        .Converters
        .Add(new JsonStringEnumConverter()));

builder.Services.AddControllersWithViews();
builder.Services.AddScoped<CreditCalculationService>();
// builder.Services.AddTransient<ICriminalChecker, CriminalChecker>();

var app = builder.Build();

if (!app.Environment.IsDevelopment())
{
    app.UseHsts();
}

app.UseHttpsRedirection();
app.UseStaticFiles();
app.UseRouting();
app.MapEndpoints();
app.MapControllerRoute(
    name: "default",
    pattern: "{controller=Home}/{action=Index}/{id?}");
app.MapFallbackToFile("index.html");
app.Run();