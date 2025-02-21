mutable struct Monomial
    n_var::Int64
    coefficent::Rational
    monomial_degree::Vector{Int64}
end

mutable struct Polynomial # TODO zero polynomial case?
    n_var::Int64
    dict_monomials::Dict{Any,Monomial}
end

struct Ideal
    generators::Vector{Polynomial}
end

# define equality of polynomials
#Base.:==(m::Monomial, n::Monomial)::Bool = (m.monomial_degree .== n.monomial_degree && m.coefficent == n.coefficent)
#Base.:==(f::Polynomial, g::Polynomial)::Bool = min(values(f.dict_monomials) .== values(g.dict_monomials))

# zero_polynomial
# TODO doesn't matter can change
zero_polynomial() = Polynomial(n_var=4, Dict{Any,Monomial}())

# define term_orders
function reversed_lex(x::Monomial,y::Monomial)
    n = length(x.monomial_degree)
    for i in 1:n
        if x.monomial_degree[n+1-i] == y.monomial_degree[n+1-i]
            continue
        elseif x.monomial_degree[n+1-i] > y.monomial_degree[n+1-i]
            return x
        else
            return y
        end
    end
end

function ell_order(x::Monomial,y::Monomial)
    ell = 3
    if sum(x.monomial_degree[1:ell]) <= sum(y.monomial_degree[1:ell]) && (reversed_lex(x,y) == y)
        return y
    else
        return x
    end
end

# monomial multiplication and addition
Base.:*(a::Monomial, b::Monomial)::Monomial = Monomial(max(a.n_var,b.n_var),a.coefficent*b.coefficent,a.monomial_degree+b.monomial_degree)
Base.:+(a::Monomial, b::Monomial)::Polynomial = add_monomials(a,b)
# division # division by 0???
Base.:/(a::Monomial, b::Monomial)::Monomial = Monomial(max(a.n_var,b.n_var),a.coefficent/b.coefficent,a.monomial_deg-b.monomial_deg)
# monomial scalarmultiplication
Base.:*(a::Monomial,b::Rational)::Monomial = scalarmultiplication(a,b)
Base.:*(b::Rational,a::Monomial)::Monomial = Base.:*(a::Monomial, b::Rational)

function add_monomials(a::Monomial,b::Monomial) # gör om...
    dict_monomials = Dict{Any,Monomial}()
    if a.monomial_degree == b.monomial_degree
        if a.coefficent+b.coefficent != 0//1
            dict_monomials[a.monomial_degree] = a.coefficent+b.coefficent
        end
    else
        dict_monomials[a.monomial_degree] = a.coefficent
        dict_monomials[b.monomial_degree] = b.coefficent
    end
    return Polynomial(max(a.n_var,b.n_var),dict_monomials)
end
# monomial and polynomial addition
Base.:+(a::Polynomial,b::Monomial)::Polynomial = add_monomial_to_polynomial(p,m)
Base.:+(a::Monomial, b::Polynomial)::Polynomial = Base.:+(b::Polynomial,a::Monomial)
# define scalarmultiplication for polynomial 
Base.:*(a::Polynomial,b::Rational)::Polynomial = scalarmultiplication(a,b)
Base.:*(b::Rational,a::Polynomial)::Polynomial = Base.:*(a::Polynomial, b::Rational)

function scalarmultiplication(a::Monomial,b::Rational)
    return Monomial(a.n_var,a.coefficent*b,a.monomial_degree)
end

function scalarmultiplication!(a::Polynomial,b::Rational) # modiferar a, detta kan skapa farliga bug
    monomials = Vector{Monomial}()
    for term in values(a.dict_monomials)
        push!(monomials,)
    end
end

function add_monomial_to_polynomial(p::Polynomial,m::Monomial)
    ## FIXME Farlig ändrar på p
    dict_monomials = p.dict_monomials
    if haskey(dict_monomials,m.monomial_degree)
        dict_monomials[m.monomial_degree] += m.coefficent
    else
        dict_monomials[m.monomial_degree] = m.coefficent
    end
    return Polynomial(max(p.n_var,m.n_var),dict_monomials)
end

# polynomial multiplication and addition
Base.:*(a::Polynomial,b::Polynomial)::Polynomial = multiply_polynomials(a,b)
Base.:+(a::Polynomial,b::Polynomial)::Polynomial = add_polynomials(a,b)
# define subtraction
Base.:-(a::Polynomial,b::Polynomial)::Polynomial = add_polynomials(a,b*(-1//1))

function remove_zero_monomials!(p::Polynomial)
    mono_dict = p.dict_monomials # i assume these have the same reference?!
    dict_keys = keys(mono_dict)
    for key in dict_keys
        if mono_dict[key].coefficent == 0
            delete!(mono_dict, key)
        end
    end
end

function add_polynomials(a::Polynomial,b::Polynomial)
    dict_monomials = Dict{Any,Monomial}()
    cur_pol = Polynomial(max(a.n_var,b.n_var),Dict{Any,Monomial}())
    # add_monomials
    for a_monomial in a.dict_monomials
        cur_pol = cur_pol + a_monomial
    end
    for b_monomial in b.dict_monomials
        cur_pol = cur_pol + b_monomial
    end
    # check if 0 entries and if so remove them
    remove_zero_monomials!(cur_sum)
    return cur_sum
end

function multiply_polynomials(a::Polynomial,b::Polynomial)
    dict_monomials = Dict{Any,Monomial}()
    for a_monomial in a.dict_monomials
        for b_monomial in b.dict_monomials
            c_monomial = a_monomial*b_monomial
            if haskey(dict_monomials,c_monomial.monomial_degree)
                dict_monomials[c_monomial.monomial_degree] += c_monomial
            else
                dict_monomials[c_monomial.monomial_degree] = c_monomial
            end
        end
    end
    p = Polynomial(max(a.n_var,b.n_var),dict_monomials)
    # check if 0 entries and if so remove them
    remove_zero_monomials!(p)
    return p
end

function LT(f::Polynomial, term_order::Function)
    # this should be fine
    leading_term = Monomial(f.n_var,1//1,[0 for i in 1:f.n_var])
    for monomial_deg in values(f.dict_monomials)
        leading_term = term_order(leading_term, monomial(f.n_var,1//1,monomial_deg))
    end
    return leading_term
end

function gcd(x::Monomial,y::Monomial)::Monomial
    #return [min(x,y) for (x,y) in zip(x.monomial_degree,y.monomial_degree)]
    return Monomial(max(x.n_var,y.n_var),1//1,min.(x.monomial_degree,y.monomial_degree)) # test if work...
end

function get_s_polynomial(f::Polynomial,g::Polynomial)
    return LT(f)/gcd(LT(f),LT(g))*f - LT(g)/gcd(LT(f),LT(g))*g
    # confirm that "-" and / is also okay
end

function is_monomial_divisible(x::Monomial,y::Monomial)
    return 0 <= min(x.monomialdegree-y.monomialdegree)
end

function is_monomial_divisible_by_some_LT(x::Monomial, G::Vector{Polynomial})
    k = length(k)
    for i in 1:k
        if is_monomial_divisible(x::Monomial,LT(G[i]))
            return i, x/LT(G[i]), (-1//1)*x/LT(G[i])*G[i]
        end
    end
    return -1, zero_polynomial(), zero_polynomial()
end

function multivariate_polynomial_division(f::Polynomial, G::Vector{Polynomial}) # glöm inte att coefficent ska vara med för 
    k = length(G)
    h_vec = [zero_polynomial() for i in 1:k]
    r = f
    r_was_changed = false
    while r_was_changed
        r_was_changed = false
        for term in values(r.dict_monomials)
            index, change_h, change_r = is_monomial_divisible_by_some_LT(term,G)
            if index != -1
                r_was_changed = true
                h_vec[i] = h_vec[i] + change_h
                r = r + change_r
                break
            end
        end
    end
    return r
end

function check_gröbner(G::Vector{Polynomial})
    n_var = G[1].n_var
    for f in G
        for g in G
            cur_NF = multivariate_polynomial_division(get_s_polynomial(f,g))
            if cur_NF != zero_polynomial()
                return cur_NF
            end
        end
    end
    return zero_polynomial()
end

# TODO is there a way to restrict which functions we can pass
function buchbergers_algorithm(I::Ideal, term_order::Function)
    G = I.generators
    cur_NF = check_gröbner(G)
    while cur_NF != zero_polynomial()
        G = vcat(G,cur_NF)
        cur_NF = check_gröbner(G)
    end
    return G
end
