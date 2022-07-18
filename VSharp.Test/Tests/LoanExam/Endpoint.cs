using System;
using System.Threading.Tasks;
using LoanExam.Models;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Routing;
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