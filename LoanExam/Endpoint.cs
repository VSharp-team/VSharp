using LoanExam.Models;
using Microsoft.AspNetCore.Mvc;
using MinimalApi.Endpoint;

namespace LoanExam;

public class Endpoint : IEndpoint<IResult, Request>
{
    public Task<IResult> HandleAsync(Request request)
    {
        throw new NotImplementedException();
    }

    public void AddRoute(IEndpointRouteBuilder app)
    {
        app.MapPost("/", ([FromBody]Request r) =>
        {
            return Results.Ok(r?.Passport?.Series);
        });
    }
}